# Code Review — Markdown Shell Extensions

**Data:** 1 settembre 2026
**Ambito:** codice proprietario del repository (`Source/`), ~15.200 righe.
**Escluso:** dipendenze vendored in `Ext/`.
**Progetto gemello:** `D:\ETHEA\MarkDownHelpViewer`, già sottoposto a review e corretto. Molte unit sono identiche o quasi; dove è così il riferimento è indicato.

**Copertura:** letti integralmente `MDShellEx.Resources.pas`, `MDShellEx.PreviewForm.pas`, `MDShellEx.ThumbnailHandler.pas`, `MDShellEx.SingleInstance.pas`, `uLogExcept.pas`, `GitHubAPI.pas`, `MDShellEx.Registry.pas`, `uRegistry.pas`, `MDShellEx.ContextMenuHandler.pas`. Di `MDTextEditor.EditorMainForm.pas` (3.023 righe) e `MDShellEx.Settings.pas` (914) sono state lette le parti rilevanti, non l'intero file.

---

## Sintesi

Il progetto condivide con MarkDownHelpViewer una parte consistente di codice, e con essa gli stessi difetti: `GitHubAPI.pas` è identico riga per riga alla versione pre-correzione, `vmHtmlToPdf.pas` è **byte-identico**, e le unit Registry / Resources / About sono varianti ravvicinate.

Ci sono però due categorie di problemi specifici di questo progetto, e sono le più importanti:

1. **Il codice gira dentro `explorer.exe`.** Preview handler e thumbnail provider sono caricati in-process: una perdita di risorse o un'eccezione non gestita lì hanno un peso diverso rispetto a un'applicazione autonoma. Il punto più grave della review è di questo tipo (§1.1).
2. **La duplicazione è raddoppiata rispetto al progetto gemello.** Oltre alle copie interne (due unit Registry distinte), c'è la duplicazione *fra i due repository*, senza alcun meccanismo che le tenga allineate.

---

## 1. Bug specifici di questo progetto

### 1.1 Perdita di GDI e di memoria dentro Explorer a ogni miniatura — `MDShellEx.ThumbnailHandler.pas:161-175`

```pascal
LBitmap := TBitmap.Create;
...
hBitmap := LBitmap.Handle;      // <- l'handle resta di proprietà del TBitmap
finally
  AStream.Free;                 // <- LBitmap non viene mai liberato
end;
```

Due problemi sovrapposti:

- **`LBitmap` non viene mai liberato.** Ogni miniatura generata lascia un `TBitmap` e le sue risorse GDI in `explorer.exe`. Sfogliando una cartella con molti file markdown la perdita si accumula nel processo della shell, che resta vivo per l'intera sessione dell'utente.
- **La proprietà dell'handle è ambigua.** Con `IThumbnailProvider` il chiamante prende possesso di `hBitmap` e prima o poi lo distrugge; il `TBitmap` però lo considera ancora suo. Oggi non si arriva alla doppia distruzione solo perché l'oggetto non viene liberato: nel momento in cui si aggiungesse il `Free` mancante senza altro, si otterrebbe una doppia `DeleteObject`.

La forma corretta è `hBitmap := LBitmap.ReleaseHandle;` seguito dalla `Free` del bitmap: `ReleaseHandle` cede l'handle e lascia l'oggetto senza risorse da distruggere.

Collegato, minore: `LBitmap.Canvas.Brush.Color` viene impostato ma nessuna `FillRect` lo applica, quindi il colore di anti-alias previsto non viene mai disegnato.

### 1.2 Il log delle eccezioni è attivo anche in release — `uLogExcept.pas:83-105`

L'overload `TLogPreview.Add(const AMessage: string)` è racchiuso in `{$IFDEF ENABLELOG}`; l'overload `TLogPreview.Add(const AException: Exception)` **no**. `ENABLELOG` è derivato da `DEBUG`, quindi in una build di release il primo overload è già inerte mentre il secondo continua a scrivere in `%TEMP%\MDShellExtensions.log`, un file che cresce senza limite e senza rotazione, scritto da dentro Explorer.

C'è anche un problema di struttura: `{$DEFINE ENABLELOG}` si trova **nella sezione implementation**, quindi non è visibile nell'interface. Le dichiarazioni di `FLogFile` e `InitLogFile` non possono essere condizionate con la stessa `{$IFDEF}` che governa i loro corpi finché il define resta lì.

Sulla direzione da prendere: chiudere anche il secondo overload nella `{$IFDEF}` è la scelta coerente, ma va detto che così una build di release non lascia più alcuna traccia diagnostica — e per una shell extension, che non ha né console né finestra di errore, quella traccia è l'unico strumento di supporto. L'alternativa migliore è tenerla attiva con un limite di dimensione e troncamento.

### 1.3 `dmResources` globale assegnato da un'istanza del preview form — `MDShellEx.PreviewForm.pas:157-170`

```pascal
constructor TFrmPreview.Create(AOwner: TComponent);
begin
  dmResources := TdmResources.Create(nil);   // variabile GLOBALE
...
destructor TFrmPreview.Destroy;
  FreeAndNil(dmResources);
```

`dmResources` è una variabile globale della unit. Se Explorer istanzia due preview handler nello stesso processo — cosa che può accadere con più finestre, o durante la transizione fra un file e l'altro — la seconda `Create` sovrascrive il puntatore.

Da lì derivano due conseguenze, a seconda dell'ordine di distruzione:

- **Perdita di memoria** (ordine B poi A): B libera il data module che ha creato e azzera il globale; la `FreeAndNil` di A non fa nulla e il data module creato da A resta allocato per sempre.
- **Uso dopo la liberazione** (ordine A poi B): A libera il data module di **B**, che B sta ancora usando — i suoi `HtmlViewer.OnImageRequest` e `OnHotSpotClick` erano stati agganciati a quell'istanza.

In più, mentre entrambe le istanze sono vive, il globale punta all'ultima creata: `MDShellEx.SettingsForm` e `dlgInputUrl`, che usano anch'essi `dmResources`, lavorano quindi sul data module e sulle impostazioni dell'istanza sbagliata.

### 1.4 Il preview form ingoia ogni eccezione senza registrarla — `MDShellEx.PreviewForm.pas:392-395`

```pascal
except
  on E: Exception do
    ; //non solleva eccezioni
end;
```

Non sollevare è corretto (un'eccezione che sfugge da un preview handler si ripercuote su Explorer), ma qui esiste già `TLogPreview`, usato ovunque nella stessa unit. Un errore di rendering della preview è al momento completamente invisibile.

### 1.5 Allocazioni inutili nel percorso di caricamento — `MDShellEx.PreviewForm.pas:279-311`

`LoadFromFile` e `LoadFromStream` creano una `TStringStream` (`LOutStream`, `LStringStream`), non la usano mai e la liberano. Codice residuo, da togliere.

Nella stessa famiglia, in `ShowMarkDownAsHTML` (sia del preview form sia dell'editor):

```pascal
LStream := TStringStream.Create(FMarkDownFile.HTML, TEncoding.UTF8);
HtmlViewer.LoadFromString(LStream.DataString);   // ritorna la stringa di partenza
```

L'intero HTML viene codificato in UTF-8 e ridecodificato per riottenere la stringa da cui si era partiti. Basta `HtmlViewer.LoadFromString(FMarkDownFile.HTML)`. Su un documento grande è una copia completa in più, per nulla.

### 1.6 Nessun fallback di codifica nel preview — `MDShellEx.PreviewForm.pas:287, 304`

`SynEdit.Lines.LoadFromFile(AFileName, TEncoding.UTF8)` senza `try/except`. L'editor un fallback ANSI ce l'ha (`TEditingFile.LoadFromFile`) e **funziona** (verificato: `TStrings.LoadFromFile` solleva davvero `EEncodingError` su UTF-8 non valido); il preview no.

Su un file markdown ANSI l'eccezione viene sollevata da `LoadFromFile` *prima* che `ShowMarkDownAsHTML` venga chiamata, quindi risale fino a `TComPreviewHandler.IPreviewHandler_DoPreview`, che la intercetta, la registra e restituisce comunque `S_OK`. Explorer crede che sia andato tutto bene e mostra un **riquadro vuoto**, senza alcun messaggio. In una build di release, dove il log è spento, il fallimento è del tutto invisibile.

La firma nel log è netta: `TFrmEditor.LoadFromFile Init` **senza** il corrispondente `Done`, seguito da `Error in TComPreviewHandler.IPreviewHandler_DoPreview`.

### 1.7 `IsTimeToCheckNewVersion` non è disattivabile — `MDShellEx.Settings.pas:618`

```pascal
Result := Now() > FVersionCheckTime + FDaysForNextCheck;
```

Manca la guardia `if FDaysForNextCheck >= 0` presente nel progetto gemello: impostando un valore negativo per disabilitare il controllo si ottiene l'effetto opposto, cioè un controllo a ogni avvio.

### 1.8 Il file temporaneo di debug in `TMarkDownFile.Parse` — `MDShellEx.Resources.pas:657-661`

```pascal
{$IFDEF DEBUG}
var LFileName := ChangeFileExt(TPath.GetTempFileName,'.html');
SaveToFile(LFileName);
{$ENDIF}
```

Solo in DEBUG, ma lascia **due** file temporanei a ogni parse: quello creato da `GetTempFileName` e quello `.html` scritto subito dopo. Durante una sessione di sviluppo con refresh automatico dell'anteprima se ne accumulano rapidamente.

### 1.9 L'editor si dichiarava una versione più vecchia di quella che è — `MDTextEditor.dproj`

```xml
<VerInfo_MinorVer>8</VerInfo_MinorVer>
<VerInfo_Release>1</VerInfo_Release>     <!-- ma FileVersion=2.8.3.0 -->
```

`VerInfo_Release` era rimasto a `1` mentre la stringa `FileVersion` diceva `2.8.3.0`. `GetCurrentVersion` legge il *fixed-file-info* binario, che è generato dagli elementi `VerInfo_*`, non dalla stringa: l'editor si dichiarava quindi **v2.8.1**.

Conseguenza concreta: il controllo aggiornamenti confrontava `v2.8.1` con la release `2.8.3` pubblicata su GitHub, trovava una versione più recente e proponeva di scaricare un aggiornamento **già installato** — a ogni controllo. Anche il titolo della finestra mostrava il numero sbagliato.

### 1.10 Il contenuto dell'anteprima è impaginato per un'area più stretta del riquadro — `uPreviewHandler.pas`, `uPreviewContainer.pas`

Segnalato dall'autore durante il collaudo: i margini del testo cambiano passando da un file all'altro e spostando Explorer fra monitor con scaling diverso, mentre i pulsanti della toolbar restano al loro posto.

Sono **tre** difetti che si sommano.

**a) `prc` viene ignorato.** Sia `SetWindow` sia `SetRect` scartano il rettangolo che l'host passa e usano quello della finestra padre:

```pascal
GetWindowRect(FParentWindow, LRect);
//Bounds is calculated on Windows Rect
Bounds := TRect.Create(0, 0, LRect.Width, LRect.Height);
```

Il contratto di `IPreviewHandler` dice che `prc` è l'area assegnata all'anteprima, espressa nelle coordinate client del padre. `GetWindowRect` restituisce l'intera finestra padre nello spazio di coordinate di *questo* processo. Con una discrepanza di DPI awareness fra finestra e monitor i due valori divergono esattamente di quel rapporto. Misurato sul log dell'autore: `prc` 996×1755 contro `GetWindowRect` 872×1536, cioè **168/192 = 0,875**. Il documento viene impaginato il 12,5% più stretto del riquadro, e il testo si ferma prima sul bordo destro.

In `SetWindow` era rimasto commentato il blocco originale, che invece usava `prc`. Il motivo del cambiamento si intuisce dal log: alla prima chiamata `prc` arriva `0×0`, e usarlo dava un'anteprima di dimensione nulla.

**b) Una seconda correzione DPI in `SetBoundsRectAndPPI`.** Quando l'anteprima è su un monitor non primario, il rettangolo veniva moltiplicato ancora per `PPI monitor corrente / PPI monitor primario`: una correzione sovrapposta a una base già sbagliata. È il motivo per cui il difetto cambia entità spostando la finestra fra monitor.

**c) Scrittura su un parametro `const`.** Nella stessa procedura, `ARect.Width := ...` su un parametro dichiarato `const`. `TRect.Width` è una property con setter, quindi il compilatore lo accetta; il record è passato per riferimento e la scrittura arriva a `FBounds` del chiamante. Il valore risulta così alterato rispetto a quello che `TComPreviewHandler.SetBounds` confronterà alla chiamata successiva.

A questo si aggiunge l'uscita anticipata di `SetBounds`:

```pascal
if Value = FBounds then Exit;
```

Derivando sempre da `GetWindowRect`, capita che il rettangolo risulti identico al precedente: in quel caso non c'è re-layout e resta l'impaginazione calcolata per una larghezza diversa. Da qui i margini che cambiano in modo apparentemente casuale da un file all'altro.

---

## 2. Difetti ereditati dal progetto gemello

Tutti già analizzati e corretti in `D:\ETHEA\MarkDownHelpViewer\CODE_REVIEW.md`; qui sono elencati con la posizione locale.

| Rif. gemello | Descrizione | Posizione qui |
|---|---|---|
| 1.14 | `ECheckNewVersionException = Exception` è un alias, non una classe: intercetta qualunque eccezione, AV comprese | `GitHubAPI.pas:41` |
| 1.15 | `FHTTPClient`/`FCustomHeaders` creati pigramente solo in `InvokeGETAsString`: AV se si chiama prima `DownloadLatestSetup` o si scrive `CustomHeaders`; `GetLatestVersionTag` non controlla `tag_name` e perde il JSON | `GitHubAPI.pas` |
| 2.6 | Nessun timeout su `THTTPClient` e controllo versione bloccante sul main thread dentro `ActionListUpdate` | `GitHubAPI.pas`, `MDTextEditor.EditorMainForm.pas:2727` |
| 1.16 | `getStreamData`: estensione `'svg'` senza punto, nome temporaneo forzato a `.svg` per qualsiasi immagine, due file temporanei mai cancellati a ogni immagine remota, ricorsione sui redirect senza limite | `MDShellEx.Resources.pas:314-395` |
| 1.16 | `LoadFileContent` ignora il valore restituito da `getStreamData` e assegna `FStream` comunque | `MDShellEx.Resources.pas:288-289` |
| 1.4 | Posizione di scroll letta **dopo** `HtmlViewer.clear`, quindi vale sempre 0 e non viene mai preservata | `MDShellEx.PreviewForm.pas:366/381`, `MDTextEditor.EditorMainForm.pas:563/568` |
| 1.7 | Chiave INI dell'orientamento PDF: si scrive `Orientation`, si rilegge `PrintOrientation` | `MDShellEx.Settings.pas:443` vs `:517` |
| 1.12 | Funzioni di *lettura* del registry che aprono la chiave con `CanCreate=True` | `MDShellEx.Registry.pas:49,68,87,106` e `uRegistry.pas:52,71,90,109` |
| 1.13 | `IsWindows11` legge un `TOSVersionInfo` mai inizializzato da `GetVersionEx` | `uRegistry.pas:140-142` |
| 2.4 | `Application.ProcessMessages` dentro la callback delle immagini, cioè in mezzo al layout HTML | `MDShellEx.Resources.pas:245` |
| 2.7 | Eccezioni ingoiate senza traccia | `MDShellEx.Resources.pas`, `uRegistry.pas`, `MDShellEx.Registry.pas` |

**Nota su 2.4:** qui pesa più che nel gemello. Nel preview handler quel `ProcessMessages` pompa la coda messaggi **di Explorer** da dentro il layout dell'HTMLViewer.

---

## 3. Duplicazione

### 3.1 Fra i due repository

| Unit | Stato |
|---|---|
| `vmHtmlToPdf.pas` | **byte-identico** a quello di MarkDownHelpViewer |
| `GitHubAPI.pas` | identico a meno della riga di copyright |
| `MDShellEx.Registry.pas` / `MDHelpView.Registry.pas` | stessa struttura, diverse nei dettagli |
| `MDShellEx.Misc.pas` / `MDHelpView.Misc.pas` | idem |
| `MDShellEx.About.pas` / `MDHelpView.About.pas` | idem, stesso `AcceptNewSetup` |
| `MDShellEx.Resources.pas` / `MDHelpView.Resources.pas` | stesso `ConvertImage` e stesso `getStreamData` |

Non esiste alcun meccanismo che tenga allineate queste copie: una correzione fatta in un repository non arriva nell'altro se non a mano. È esattamente quello che è successo — i difetti sopra elencati esistono in due esemplari.

La soluzione strutturale è una libreria condivisa (un repository, o un `svn:externals` come già si fa per `Ext/`) che contenga almeno `GitHubAPI`, `vmHtmlToPdf`, le utility Registry/Misc e il codice di caricamento e conversione immagini.

### 3.2 Interna al progetto

- **`MDShellEx.Registry.pas` e `uRegistry.pas`** offrono le stesse quattro funzioni `RegReadStr` / `RegReadInt` / `RegWriteStr` / `RegWriteInt`, con lo stesso difetto in entrambe.
- `TryLoadTextFile` in `MDShellEx.Misc.pas:228` è **dichiarata e definita ma non usata da nessuno**: codice morto.
- `TvmHtmlToPdf` (la variante non-GDI) non è usata da nessuno, ed è al 95% identica a `TvmHtmlToPdfGDI`. La sua `Execute` non ha `try/finally`: se qualcosa solleva nel ciclo, la `TList` e tutti i `TMetafile` restano allocati. *(Presente anche nel gemello e non ancora corretta lì.)*

---

## 4. Cose fatte bene

Vanno segnalate, perché in alcuni casi questo progetto è **avanti** rispetto al gemello e la direzione giusta è portare le sue soluzioni nell'altro, non il contrario.

- **`MDShellEx.SingleInstance.pas` + `TfrmMain.WMCopyData`** sono l'implementazione corretta dell'IPC: nome di classe finestra dedicato, firma `dwData` verificata, `cbData` e `lpData` verificati, `Msg.Result` impostato, payload `PChar` Unicode (niente `ShortString` con conversione ANSI lossy), e `AllowSetForegroundWindow` chiamata dal mittente. È esattamente il modello a cui ho ricondotto MarkDownHelpViewer, che partiva da un record `ShortString`.
  L'unico neo: `SendFileToExistingInstance` restituisce `True` senza guardare il risultato della `SendMessage`, quindi se il destinatario non gestisse il messaggio il file non verrebbe aperto in silenzio. Ora che il ricevente imposta `Msg.Result`, la verifica è gratuita.
- **`TEditingFile.Destroy`** toglie il focus prima di distruggere i controlli, con il commento che spiega l'AV su Win64 che si evita così.
- **`TMarkDownFile.Parse`** aggancia e sgancia correttamente il `codeBlockEmitter` prima di liberare il processor, con la spiegazione del perché.
- **`GetThumbnail`** e i metodi COM incapsulano tutto in `try/except` restituendo `E_FAIL`: è il comportamento giusto per un'interfaccia COM (il difetto è la gestione dell'handle, non la struttura).
- **I numeri di versione sono allineati**: `2.8.3` in tutti e tre i `.dproj`, nell'`.iss` e nel README. Nel gemello erano su quattro valori diversi. *(Con un'eccezione, vedi §1.9.)*

---

## 5. Minori

- `TfrmMain.OpenFile` solleva un messaggio in inglese cablato nel codice (`'Cannot open file with extensions different from "%s"'`) mentre il resto dell'applicazione usa il sistema di traduzione.
- `MDShellEx.ContextMenuHandler.pas:213` legge la chiave con `Reg.OpenKey(..., False)` — corretto — ma non verifica l'esito di `ReadString`; se la chiave esiste vuota si arriva a una `ShellExecute` con comando vuoto.
- `TFrmPreview.LoadFromStream` imposta `HtmlViewer.ServerRoot` alla cartella della DLL: le immagini relative di un documento aperto da stream vengono cercate accanto alla DLL.
- `TComMDThumbnailProvider.GetThumbnail`: `Result` non è assegnato sul ramo `cx = 0`… in realtà lo è (`S_FALSE`), ma `bitmapType` no; il chiamante legge un valore non inizializzato. Va impostato prima dell'uscita anticipata.

---

## Priorità

### Gruppo A — da correggere subito

| # | Descrizione | Perché prima |
|---|---|---|
| 1.1 | Handle e `TBitmap` della miniatura | perdita di risorse dentro `explorer.exe` |
| 1.3 | `dmResources` globale nel preview form | rischio di puntatore penzolante con più istanze |
| 1.2 | Log delle eccezioni attivo in release | file che cresce senza limite |
| 2.§ | Porting delle correzioni dal gemello: `GitHubAPI` (1.14, 1.15, 2.6), `getStreamData` (1.16), posizione di scroll (1.4), chiave PDF (1.7), registry (1.12), `IsWindows11` (1.13) | difetti già analizzati e già corretti altrove |

### Gruppo B — a seguire

1.4 (log del preview), 1.5 (allocazioni inutili), 1.6 (fallback codifica nel preview), 1.7 (guardia sul check versione), 1.8 (file temporanei in DEBUG), 2.4 (`ProcessMessages`), più i punti del §5.

### Gruppo C — strutturale

Unificare il codice condiviso fra i due repository (§3.1) e le due unit Registry (§3.2). Finché resta duplicato, ogni correzione va applicata due volte — ed è il motivo per cui questa review ripete in buona parte la precedente.

---

## Stato

### Corretto (1 settembre 2026)

Compilati con successo tutti e quattro i target: `MDTextEditor` Win64 e Win32, `MDShellExtensions` Win64, `MDShellExtensions32` Win32. Nessun hint proveniente dal codice del progetto (restano i due preesistenti di `Ext/`).

| # | Intervento | File |
|---|---|---|
| 1.1 | `hBitmap := LBitmap.ReleaseHandle` + `Free` del bitmap; `bitmapType` impostato prima dell'uscita anticipata; aggiunta la `FillRect` che rende effettivo il colore di anti-alias | `MDShellEx.ThumbnailHandler.pas` |
| 1.2 | Anche l'overload `Add(Exception)` è sotto `{$IFDEF ENABLELOG}`; il `{$DEFINE ENABLELOG}` è stato spostato dalla implementation all'interface, così dichiarazioni e corpi sono governati dalla stessa condizione | `uLogExcept.pas` |
| 1.4 | Le eccezioni del preview vengono registrate con `TLogPreview.Add(E)` invece di essere scartate in silenzio | `MDShellEx.PreviewForm.pas` |
| 1.5 | Rimosse le `TStringStream` inutilizzate e il giro UTF-8 andata/ritorno: `HtmlViewer.LoadFromString(FMarkDownFile.HTML)` diretto | `MDShellEx.PreviewForm.pas`, `MDTextEditor.EditorMainForm.pas` |
| 1.7 | Guardia `FDaysForNextCheck < 0` in `IsTimeToCheckNewVersion` | `MDShellEx.Settings.pas` |
| 2.§ 1.14/1.15/2.6 | `GitHubAPI.pas` sostituita con la versione corretta del progetto gemello (classe di eccezione reale, `EnsureClient`, timeout, `GetLatestVersionTag` robusta). Conservata la riga di copyright locale | `GitHubAPI.pas` |
| 2.§ 2.6 | Nuova `CheckNewSetupAsync` e controllo automatico all'avvio spostato in `TTask` | `MDShellEx.About.pas`, `MDTextEditor.EditorMainForm.pas` |
| 2.§ 1.16 | `getStreamData` riscritta: nessun file temporaneo, decoder scelto dall'estensione dell'URL scaricato, redirect manuali limitati a 5; `LoadFileContent` usa il valore restituito | `MDShellEx.Resources.pas` |
| 2.§ 1.4 | Posizione di scroll letta prima di `Clear` | `MDShellEx.PreviewForm.pas`, `MDTextEditor.EditorMainForm.pas` |
| 2.§ 1.7 | Chiave INI dell'orientamento PDF allineata a `PrintOrientation` | `MDShellEx.Settings.pas` |
| 2.§ 1.12 | `OpenKeyReadOnly` nelle funzioni di lettura, in entrambe le unit | `MDShellEx.Registry.pas`, `uRegistry.pas` |
| 2.§ 1.13 | `IsWindows11` non legge più un record non inizializzato | `uRegistry.pas` |

### 1.3 — `dmResources` singleton (applicato in un secondo momento)

`dmResources` non è più una variabile globale scritta dai consumatori, ma una **funzione** che restituisce un'istanza condivisa, creata alla prima richiesta e liberata nella `finalization` della unit che la possiede.

Averla esposta come funzione anziché come variabile è ciò che ha reso il cambiamento contenuto: tutti e diciassette i punti che scrivono `dmResources.Qualcosa` in `MDShellEx.PreviewForm`, `MDShellEx.SettingsForm`, `MDTextEditor.EditorMainForm` e `dlgInputUrl` compilano invariati. Sono cambiati solo i due punti che la creavano.

| Intervento | File |
|---|---|
| `function dmResources: TdmResources` con creazione pigra; `_dmResources` privata alla unit; `finalization` che la libera | `MDShellEx.Resources.pas` |
| `function dmResourcesCreated: Boolean`, che verifica l'esistenza **senza** crearla | `MDShellEx.Resources.pas` |
| `RescalingImageEnabled`, nil-safe, al posto di `Settings.RescalingImage` nei tre punti di `ConvertImage` | `MDShellEx.Resources.pas` |
| Il costruttore non crea più il data module; il distruttore non lo libera più e si limita a sganciare `Settings` se punta ancora alle proprie | `MDShellEx.PreviewForm.pas` |
| Rimossa `Application.CreateForm(TdmResources, dmResources)` | `MDTextEditor.dpr` |

Due dettagli non ovvi:

- **`dmResourcesCreated` non è ridondante.** Siccome `dmResources` crea l'istanza su richiesta, un `Assigned(dmResources)` in un distruttore la farebbe rinascere durante la chiusura dell'applicazione. Il distruttore del preview form deve poter dire "esiste già?" senza provocarne la creazione.
- **`Settings` va sganciato.** Il data module ora sopravvive ai form: se un preview form libera le proprie `TPreviewSettings` lasciando `dmResources.Settings` a puntarci, resta un puntatore penzolante. Il distruttore lo azzera, e i tre usi dentro `ConvertImage` sono diventati nil-safe.

**Verifica:** compilati tutti e quattro i target. L'editor Win64 è stato costruito in una cartella separata (l'eseguibile in `Bin64` era bloccato da un'istanza in esecuzione) e avviato: si apre, resta stabile, e si chiude con `WM_CLOSE` senza errori — cioè la `finalization` libera il data module correttamente.

**Non verificato a runtime:** il percorso della DLL. Nel preview handler la `finalization` viene eseguita allo scaricamento della libreria da parte di Explorer, non più durante la distruzione del form: è un cambiamento di tempistica che andrebbe validato registrando la DLL e sfogliando una cartella con file markdown.

### Versione allineata a 2.8.4 (1 settembre 2026)

Incremento di patch, coerente con un rilascio di sole correzioni.

| Dove | Prima | Dopo |
|---|---|---|
| `Source/MDShellExtensions.dproj` | 2.8.3.0 | 2.8.4.0 |
| `Source/MDShellExtensions32.dproj` | 2.8.3.0 | 2.8.4.0 |
| `Source/MDTextEditor.dproj` | stringa 2.8.3.0, **binario 2.8.1.0** | 2.8.4.0 in entrambi |
| `Setup/MDShellExtensions.iss` (`MyAppVersion`) | 2.8.3 | 2.8.4 |

Questo chiude anche **§1.9**: l'editor non si dichiara più una versione più vecchia di quella che è.

Verificato sui binari prodotti: `MDTextEditor.exe` e `MDShellExtensions.dll` (Win64 e Win32) riportano `2.8.4.0` sia nell'informazione binaria sia nella stringa.

### Gruppo B completato (1 settembre 2026)

| # | Intervento | File |
|---|---|---|
| 1.6 | Fallback ANSI su `LoadFromFile` e `LoadFromStream` dell'anteprima, come già faceva l'editor. Nella variante da stream lo stream viene riavvolto prima del secondo tentativo | `MDShellEx.PreviewForm.pas` |
| 1.5 | Rimosse le ultime `TStringStream` create, mai usate e liberate in `LoadFromFile` / `LoadFromStream` | `MDShellEx.PreviewForm.pas` |
| 1.8 | Il dump HTML di debug scrive un unico file a nome fisso (`%TEMP%\MDShellExtensions_LastParsed.html`) sovrascritto a ogni parse, invece di due file orfani per volta | `MDShellEx.Resources.pas` |
| 2.4 | Nuovo flag `PumpMessagesWhileLoading` su `TdmResources`: la `Application.ProcessMessages` nel caricamento immagini viene eseguita **solo nell'editor**, mai nella shell extension | `MDShellEx.Resources.pas`, `MDTextEditor.EditorMainForm.pas` |
| 2.4 | `UpdateMDViewer` alza `FProcessingFiles` per tutta la durata del rendering, non lo legge soltanto in ingresso | `MDTextEditor.EditorMainForm.pas` |
| 2.4 | Guardia di rientranza `FRendering` in `TFrmPreview.ShowMarkDownAsHTML`, con `try/finally` | `MDShellEx.PreviewForm.pas` |
| §5 | Il messaggio "Cannot open file with extensions..." è diventato la `resourcestring EXTENSION_NOT_ACCEPTED`, come il resto dell'unit | `MDTextEditor.EditorMainForm.pas` |
| §5 | Il menu contestuale verifica che il comando letto dal registry non sia vuoto: prima una chiave presente ma vuota arrivava a `ShellExecute` senza alcun riscontro per l'utente | `MDShellEx.ContextMenuHandler.pas` |
| §5 | Caricamento da stream: `ServerRoot` lasciato vuoto invece che puntato alla cartella della DLL, dove un'immagine relativa del documento avrebbe pescato quel che c'era accanto alla libreria | `MDShellEx.PreviewForm.pas` |

Sul punto 2.4 vale la pena spiegare la scelta. Nel progetto gemello avevo mantenuto la `ProcessMessages` perché è ciò che rende reattiva la UI e permette l'annullamento con ESC. Qui la stessa riga serve **due** ospiti diversi: nell'editor quel compromesso ha senso, nel preview handler no — non c'è alcun comando di annullamento e la coda che verrebbe pompata è quella di Explorer. Da qui il flag, spento per default e acceso solo dall'editor. Le guardie di rientranza restano su entrambi i lati.

L'editor aveva già il meccanismo giusto (`FProcessingFiles`, usato correttamente da `ApplyViewerZoom` con tanto di commento sul perché); mancava solo che `UpdateMDViewer` lo impostasse invece di limitarsi a leggerlo.

**Verifica:** compilati tutti e quattro i target. Editor Win64 avviato su `README.md` (che contiene badge remoti): parte, resta stabile, nessun file temporaneo creato. Il lancio di una seconda istanza con un file ANSI inoltra correttamente via `WM_COPYDATA` e termina, lasciando una sola istanza che apre il file senza errori. Chiusura pulita.

**Non verificato a runtime:** il percorso della DLL (preview handler e menu contestuale), che richiede la registrazione con `regsvr32` e Explorer chiuso.

### 1.10 — dimensionamento DPI dell'anteprima (1 settembre 2026)

| Intervento | File |
|---|---|
| `SetWindow` usa `prc` quando non è vuoto; `GetWindowRect` resta solo come ripiego per la prima chiamata, dove l'host passa `0×0` | `uPreviewHandler.pas` |
| `SetRect` segue la stessa regola | `uPreviewHandler.pas` |
| `SetRect` forza `UpdateContainerBoundsRect` quando il rettangolo è invariato ma il DPI è cambiato: l'assegnazione a `Bounds` uscirebbe subito, lasciando l'impaginazione del DPI precedente | `uPreviewHandler.pas` |
| Rimossa la seconda correzione per monitor non primario, ora superflua e in conflitto con la prima | `uPreviewContainer.pas` |
| Eliminata la scrittura sul parametro `const ARect`, che alterava `FBounds` del chiamante | `uPreviewContainer.pas` |
| Il log riporta il PPI del monitor e se è quello primario, utile per diagnosticare i casi multi-monitor | `uPreviewContainer.pas` |

Compilano sia in Debug sia in Release, Win64 e Win32.

**Verificato dall'autore**, riscontro visivo positivo e log a conferma, includendo un passaggio reale fra monitor con scaling diverso:

```
16:00:03  monitor secondario, 192 DPI, Primary: 0
          prc 996×1755  ->  Bounds 996×1755  ->  ARect 996×1755
          (GetWindowRect diceva 872×1536: è il valore che si usava prima)

16:00:18  monitor primario, 168 DPI, Primary: -1
          prc 872×1395  ->  Bounds 872×1395  ->  ARect 872×1395
```

Il primo caso è quello dirimente: monitor **non primario**, dove il codice precedente sommava entrambe le correzioni. Il valore ora attraversa la catena immutato in entrambe le configurazioni, e nessuna eccezione compare nel log. Confermata anche l'uscita anticipata: un `SetRect` con rettangolo e DPI invariati non produce alcun `SetBounds`.

Resta **non esercitato** il ramo che forza il re-layout a rettangolo invariato e DPI cambiato: al passaggio fra monitor è cambiato anche il rettangolo, quindi si è preso il percorso normale. Quel ramo copre il caso più stretto — modifica della percentuale di scaling in Impostazioni con l'anteprima aperta e il riquadro delle stesse dimensioni — e non ha ancora una prova sul campo.

### Gruppo C — pulizia interna (1 settembre 2026)

Scelta dell'autore: rimuovere codice morto e duplicati **interni** a ciascun repository, rimandando la condivisione fra i due progetti (§3.1). Le copie restano quindi allineate a mano, e per questo ogni modifica qui sotto è stata applicata **a entrambi** dove il file è condiviso.

| Intervento | Effetto |
|---|---|
| Eliminata `MDShellEx.Registry.pas` (`svn delete`) e rimossa dai tre `.dpr` e dai tre `.dproj` | Era una copia indipendente di `uRegistry.pas`, **mai chiamata**: compariva solo nelle clausole `uses` dei progetti. I consumatori reali (`Settings`, `SettingsForm`, `ThumbnailHandler`) usano `uRegistry` |
| Rimossa la classe `TvmHtmlToPdf` da `vmHtmlToPdf.pas` **nei due repository** | Mai istanziata in nessuno dei due, copia al ~95% di `TvmHtmlToPdfGDI`, ed era quella con l'`Execute` priva di `try/finally`. **476 → 244 righe**, e i due file restano byte-identici |
| Rimossa `TryLoadTextFile` da `MDShellEx.Misc.pas` | Dichiarata e implementata, chiamata da nessuno |
| Il log della miniatura riporta la dimensione dell'SVG invece del suo contenuto | L'icona è identica per ogni file markdown: con 47 miniature quel `TLogPreview.Add` produceva il **91%** del file di log |

Nota su `svn delete --force`: `MDShellEx.Registry.pas` aveva la modifica locale `OpenKeyReadOnly` fatta durante il Gruppo A. Forzare è sicuro perché la stessa correzione era stata applicata anche a `uRegistry.pas`, la copia che sopravvive.

Compilano tutti i target dei due progetti, Debug e Release, Win64 e Win32.

### Non applicato

- **§3.1 condivisione fra i due repository**: rimandata per scelta. `vmHtmlToPdf.pas` (244 righe, byte-identico) e `GitHubAPI.pas` (349 righe, differenti per la sola riga di copyright) sono pronti a essere estratti quando si deciderà dove collocarli. Le unit analoghe ma divergenti — Registry (95 righe diverse), About (95), Misc (260) — richiedono prima una riconciliazione caso per caso, non sono una semplice estrazione.
- **`ReadSettings` letto quattro volte per anteprima**: le chiamate arrivano dal costruttore e da due punti del preview form, non è codice morto. Toglierne una richiede capire quale aggiornamento garantisce; non l'ho fatto alla cieca.
- **Changelog del README**: va aggiunta a mano la voce per la 2.8.4; il contenuto è una scelta editoriale.

### Configurazione della DLL a 32 bit

`MDShellExtensions32.dproj` con `/p:config=release` compilava i rami sotto `ENABLELOG` (derivato da `DEBUG`): la DLL a 32 bit veniva quindi rilasciata con semantica di debug — log completo e, per il §1.8, due file temporanei a ogni anteprima. **Corretto dall'autore.**

---

## Correzioni riportate da SVGShellExtensions — 2 settembre 2026

La review dell'editor SVG (`D:\ETHEA\SVGShellExtensions\CODE_REVIEW.md`, § 1.2 e § 1.3) ha trovato due difetti nella gestione della memoria di `AddEditingFile` che sono **anche** in `MDTextEditor.EditorMainForm.pas`, in forma più estesa.

### Doppia liberazione nel gestore di eccezioni

```pascal
  Except
    LTabSheet.Free;
    LEditor.Free;      // <-- distrutto già da LTabSheet.Free
    LFEViewer.Free;    // <-- idem
    raise;
  End;
```

`LEditor`, `LFEViewer` e `LSplitter` sono tutti figli di `LTabSheet`. Il punto non era ovvio perché qui `TSynEdit` e `THtmlViewer` sono creati con `Create(**nil**)` — owner nil, solo `Parent` impostato — quindi sembrerebbe che il TabSheet non li possieda.

Verificato compilando: **`TWinControl.Destroy` distrugge i controlli figli indipendentemente dalla proprietà.** Un figlio creato con `Create(nil)` e `Parent` impostato viene distrutto dal parent esattamente come uno di cui il parent è owner. Quindi le due `Free` successive operavano su puntatori pendenti.

Ora l'`except` libera solo `LTabSheet` e azzera `EditingFile.SynEditor`, `.HTMLViewer`, `.Splitter`, `.TabSheet`.

### `EditFileList.Add` prima del `try`

`Result := EditFileList.Add(EditingFile)` era la prima istruzione, fuori dal `try`, con `EditFileList` creata come *owns objects*. I due chiamanti assumono il contrario:

- **`OpenFile`**: se `AddEditingFile` solleva, `I` è ancora `-1`, quindi si eseguiva `EditingFile.Free` su un oggetto che la lista già possedeva. Inoltre `I >= 0` era vero anche per un file **già aperto**, quindi un ricaricamento fallito chiudeva la scheda esistente dell'utente. Introdotta `LCreated` per distinguere i due casi.
- **`acNewFileExecute`**: `SetFocus` stava dentro il `try` che libera l'oggetto in caso di errore, quindi un fallimento *dopo* il passaggio di proprietà produceva una seconda liberazione. Spostata fuori.

`EditFileList.Add` è ora l'ultima istruzione di `AddEditingFile`: la proprietà passa alla lista solo quando tutto è riuscito, e il chiamante resta l'unico proprietario nel percorso di errore.

### Difetto del `.dproj`

`MDTextEditor.dproj` ha `<DCC_DebugInformation>true</DCC_DebugInformation>` nel `PropertyGroup` di `Cfg_2`, mentre i valori ammessi sono numerici. Qui era **mascherato** perché `Cfg_2_Win32` e `Cfg_2_Win64` lo sovrascrivono con `2`; negli omologhi di SVGShellExtensions, privi di quell'override, il build Debug falliva con `error F1026: File not found: 'true.dpr'`. Corretto a `2` anche qui.

Compilano Debug e Release Win64, senza warning né hint nuovi.

---

## Regressione del check versione asincrono — 2 settembre 2026

**Sintomo:** Access Violation in `System._UStrAsg` aprendo la form About dal
controllo automatico di nuova versione.

```
System._UStrAsg(???,???)
GitHubAPI.TGitHubHttpClient.DownloadLatestSetup('...Setup.exe',(...HttpClientReceiveData,$38766A0),'')
uAbout.TFrmAbout.DownloadAndInstallSetup
uAbout.TFrmAbout.FormActivate($38766A0)
...
Vcl.Forms.TCustomForm.ShowModal
uAbout.ShowAboutForm(..., True)
EditorMainForm.TfrmMain.ActionListUpdate$ActRec.$0$Body('v3.4.0','v3.4.1')
uAbout.CheckNewSetupAsync$ActRec.$0$Body$ActRec.$0$0$Body
System.Classes.CheckSynchronize(???)
```

**Causa.** La variabile globale `_HttpClient` veniva creata **solo** dentro
`AcceptNewSetup`, cioè il percorso del controllo **manuale**:

```pascal
if not Assigned(_HttpClient) then
  _HttpClient := TGitHubHttpClient.Create(nil);
Result := _HttpClient.IsNewVersionAvailable(...);
```

mentre `DownloadAndInstallSetup` la usava senza verificarla. Finché il controllo
era sincrono la sequenza era sempre "controlla poi scarica", quindi il client
esisteva per effetto collaterale. Spostando il controllo automatico in un task
di background — che per non condividere stato fra thread usa un client locale
suo — nessuno crea più `_HttpClient`: la form About aperta direttamente dal
callback arriva a `DownloadLatestSetup` con `Self = nil`, e la prima
assegnazione di campo (`FSetupFileName := ASetupFileName`) scrive a un offset
da `nil`. Da qui il frame `System._UStrAsg`.

Il terzo argomento mostrato come `''` nel trace è il parametro `out
ADownloadedFileName`, non la causa: è vuoto perché la funzione non era ancora
arrivata ad assegnarlo.

**Secondo difetto sullo stesso percorso.** Anche creando il client, un'istanza
appena costruita ha `FGitHubProjectURL` vuoto, e `DownloadLatestSetup` lo
verificava solo con un `Assert` **sul nome del file**, non sull'URL. Il
risultato non sarebbe stato un errore ma la URL malformata
`/releases/latest/download/<setup>`.

**Terzo difetto: il download fallito si ritentava in loop.** `FormActivate`
avvia il download quando `FStopDownload` è `False`, e `OnActivate` scatta a
ogni riattivazione della form. `UpdateGUI(False)`, chiamata nel `finally`,
rimetteva la didascalia del pulsante ma **non** il flag: chiuso il dialogo
d'errore, la form si riattivava e il download ripartiva.

### Correzioni

| Dove | Modifica |
|---|---|
| form About (3 progetti) | Nuova `function GitHubClient: TGitHubHttpClient` che crea il client alla prima chiamata **e** ne imposta sempre `GitHubProjectURL`. Usata sia da `AcceptNewSetup` sia da `DownloadAndInstallSetup` |
| form About (3 progetti) | `UpdateGUI(False)` rimette `FStopDownload := True`, coerentemente con la didascalia che torna a "check for updates" |
| `GitHubAPI.pas` (3 copie) | I due `Assert` sui parametri obbligatori diventano controlli veri con `ECheckNewVersionException` e messaggi propri (`ERR_SETUP_FILENAME_MISSING`, `ERR_PROJECT_URL_MISSING`): un `Assert` può essere escluso dalla compilazione, e sull'URL vuoto non c'era alcun controllo |

La regressione era presente in tutti e tre i progetti che hanno ricevuto
`CheckNewSetupAsync` — MarkDownHelpViewer, MarkDownShellExtensions e
SVGShellExtensions — e si manifesta solo quando una versione più recente esiste
davvero, per questo non era emersa prima.

### Osservazioni non modificate

- Il download parte da `FormActivate` e gira quindi dentro la closure accodata
  da `TThread.Queue`, con una form modale annidata. Funziona, ma un pulsante
  esplicito o un `PostMessage` alla form sarebbe più lineare.
- `DownloadAndInstallSetup` confronta la dimensione scaricata con
  `FExpectedSize`, che viene valorizzato dal primo callback `OnReceiveData` con
  `AContentLength`. Se il server rispondesse senza `Content-Length` il
  confronto fallirebbe sempre e un download riuscito sarebbe segnalato come
  errore. GitHub lo invia, quindi in pratica non si verifica.

### Regex delle versioni allineata a kittoX

`DecodeVersion` usava `'^v(\d+)\.(\d+)\.(\d+)$'`, che impone il prefisso `v`
e vieta qualunque suffisso. La copia in `kittoX/Kide/Source/GitHubAPI.pas` era
gia' stata resa piu' tollerante, e ora le quattro copie sono identiche:

```pascal
// Optional 'v'/'V' prefix and an ignored trailing suffix (e.g. a pre-release
// tag like 'beta' in 'V4.0.18beta'): only the leading N.N.N is significant.
Regex := TRegEx.Create('^v?(\d+)\.(\d+)\.(\d+)', [roIgnoreCase]);
```

Con la versione precedente un tag come `3.4.2`, `V4.0.18beta` o `v3.4.2-rc1`
faceva sollevare `ECheckNewVersionException`: nel controllo automatico
l'eccezione viene inghiottita in silenzio e l'aggiornamento non viene **mai**
proposto, nel controllo manuale l'utente vede il dialogo di errore con
"retry". Il rischio riguarda solo il tag che arriva da GitHub, perche'
`GetCurrentVersion` produce sempre `vN.N.N`.

Comportamento verificato compilando un confronto fra le due regex
(`dcc64` 37.0):

| tag | vecchia | nuova |
|---|---|---|
| `v3.4.2`, `V3.4.2` | 3.4.2 | 3.4.2 |
| `v3.4.10` | 3.4.10 | 3.4.10 |
| `v10.20.30` | 10.20.30 | 10.20.30 |
| `3.4.2` | eccezione | **3.4.2** |
| `V4.0.18beta` | eccezione | **4.0.18** |
| `v3.4.2-rc1` | eccezione | **3.4.2** |
| `v2.5` | eccezione | eccezione |
| `release-3.4.2` | eccezione | eccezione |
| `''`, `vX.Y.Z` | eccezione | eccezione |

Nessun tag che prima veniva accettato cambia risultato, e togliere l'ancora
`$` non intacca la greediness di `\d+`: `v3.4.10` resta `3.4.10` e non
`3.4.1`. L'ancora `^` continua a respingere i prefissi non numerici come
`release-3.4.2`.

Effetto collaterale da tenere presente: un tag di pre-release viene ora
interpretato come la versione finale corrispondente, quindi `v3.4.3-rc1`
sarebbe proposto come `3.4.3`. In pratica non capita perche' l'endpoint
`releases/latest` di GitHub esclude le pre-release.

---

## Allineamento dell'editor markdown alla review dell'editor SVG — 2 settembre 2026

La nota di copertura in testa a questo documento dice che
`MDTextEditor.EditorMainForm.pas` (3.023 righe) era stato letto **solo nelle
parti rilevanti**. La review sistematica dell'editor SVG ha prodotto un elenco
puntuale di difetti, e il confronto ha mostrato che i due editor erano
disallineati in entrambe le direzioni.

**Già corretti qui, e meglio che nell'editor SVG:** `CanAcceptFileName` usa
`IsFileNameWithExt`; `LoadOpenedFiles` itera `EditFileList` invece di
indicizzarlo con l'indice delle impostazioni; `RemoveEditingFile` ricava la
pagina dall'oggetto e documenta l'ordine di distruzione. Queste ultime due sono
state portate **da qui all'editor SVG**.

**Corretti ora, portati dall'editor SVG:**

| § | Intervento |
|---|---|
| 1.4 | `ConfirmChanges` e `ModifiedCount` verificano `Assigned(SynEditor)`: il percorso `OpenFile` → `except` → `RemoveEditingFile` può arrivarci con il campo ancora nil |
| 1.7 | `acSaveAllExecute` con `try/except` per file e messaggio riepilogativo (`FILES_NOT_SAVED`): il primo file non salvabile interrompeva il ciclo e lasciava gli altri modificati senza dirlo |
| 3.1 | `WMCopyData` limita `cbData` a 32767 caratteri e tronca al primo `#0` invece di fidarsi della lunghezza dichiarata |
| 3.2 | Rimosso `mbHelp` da `ManageExceptions`: `MDTextEditor.dpr:81` imposta `Application.HelpFile := ''`, quindi il pulsante compariva e non faceva nulla |
| 3.3 | Aggiunto `mbCancel` a `ConfirmChanges` e `FormClose` intercetta `EAbort` impostando `Action := caNone`: la chiusura con file modificati non era annullabile e il ramo `Abort` era codice morto |

`MDTextEditor.dproj` Win64, Debug e Release: OK.

Per i dettagli su § 3.3 (perché `mrCancel` arriva davvero, verificato su
`Vcl.StyledTaskDialog.pas`) e su § 3.1 (perché la descrizione originale del
difetto era imprecisa) vedi
`D:\ETHEA\SVGShellExtensions\CODE_REVIEW.md`, sezione "Gruppo C applicato".

---

## Il gutter di SynEdit non seguiva lo stile VCL — 2 settembre 2026

**Sintomo:** passando al tema scuro il gutter resta grigio chiaro con i numeri
di riga neri, mentre l'area di testo diventa correttamente scura.

### Causa: SynEdit non e' affatto style-aware

`SynEdit.pas` **non consulta mai `StyleServices`**. In tutta la libreria solo
tre unit lo fanno — `SynCompletionProposal.pas`, `SynEditScrollBars.pas` e
`SynEditMiscClasses.pas`, quest'ultima per le sole annotazioni della barra di
scorrimento. Il gutter non e' coinvolto.

`TSynGutter.Create` inizializza i colori con costanti di sistema **grezze**:

```pascal
FColor := clBtnFace;
FBorderColor := clWindow;
FGradientStartColor := clWindow;
FGradientEndColor := clBtnFace;
```

Senza passare da `StyleServices.GetSystemColor`, `clBtnFace` resta il grigio
di Windows (`#F0F0F0`) e il font del gutter il nero di sistema, qualunque sia
lo stile VCL attivo.

**Perche' allora l'area di testo funziona?** Perche' non e' SynEdit a farlo, e'
l'applicazione: `UpdateHighlighter` calcola
`StyleServices.GetSystemColor(clWindow)` e lo stampa sul `Background` di
**ogni** attributo dell'highlighter. Il gutter, che non e' coperto da nessun
attributo, non veniva toccato da nessuno. E `uSettings.pas` non legge ne'
scrive **alcun** colore del gutter, quindi non c'era nemmeno una preferenza
utente a coprire il caso.

### I quattro colori che il gutter disegna davvero

Prima di scegliere cosa impostare ho cercato dove ciascun colore viene letto in
fase di disegno:

| colore | letto in |
|---|---|
| `Gutter.Color` | `SynEdit.pas:2468` — `TSynDWrite.SolidBrush(fGutter.Color)`, fondo piatto |
| `Gutter.GradientStartColor` / `GradientEndColor` | `SynEdit.pas:2462-2463`, quando `Gradient` e' attivo |
| `Gutter.BorderColor` | `SynEditMiscClasses.pas:2386` |
| `Gutter.Font.Color` | `SynEditMiscClasses.pas:2297`, **solo se `UseFontStyle` e' True** |

L'ultima riga e' il dettaglio che conta. `PaintLineNumbers` fa:

```pascal
if Gutter.UseFontStyle then
begin
  TextFormat := Gutter.FTextFormat;
  FontColor := Gutter.Font.Color;
end
else
begin
  TextFormat := SynEdit.TextFormat;
  FontColor := SynEdit.Font.Color;       // <-- ripiego sul font dell'editor
end;
```

Quindi impostare solo `Gutter.Font.Color` avrebbe lasciato il difetto in piedi
per chi disattiva `UseFontStyle` nelle opzioni dell'editor. Per questo viene
impostato anche il colore del font dell'editor.

### Correzione

Nuovo metodo `ApplyStyleToGutter(AGutter: TSynGutter)` in entrambi gli editor,
che risolve i cinque colori attraverso `StyleServices.GetSystemColor`, cioe' la
stessa API che l'applicazione usa gia' per l'area di testo.

Chiamato in **due** punti, perche' le due rotte del cambio tema sono distinte:

| punto | copre |
|---|---|
| `InitEditorOptions`, su `FEditorOptions.Gutter` | tutti gli editor, perche' `AssignTo` fa `Gutter.Assign` e copia anche `Font` e `Color` |
| `UpdateHighlighter`, sul gutter dell'editor passato | `UpdateHighlighters`, che rinfresca ogni editor aperto **senza** passare dal contenitore delle opzioni |

In `InitEditorOptions` vengono impostati anche `Font.Color` e `Color` del
contenitore. Il secondo chiude un caso vicino nell'editor markdown: quando
`HighlightMarkdownText` e' disattivato l'highlighter viene staccato
(`Highlighter := nil`) e il fondo del testo arriva da `TCustomSynEdit.Color`,
che con i colori grezzi sarebbe rimasto chiaro anche in tema scuro.

L'ordine in `UpdateFromSettings` e' quello giusto senza modifiche:
`UpdateApplicationStyle` cambia lo stile **prima** di `InitEditorOptions`,
quindi `GetSystemColor` restituisce gia' i colori del nuovo stile.

### Verifica

Compilato un programma che carica gli 81 file `.vsf` disponibili (redist di RAD
Studio piu' CatalogRepository) e per ogni stile registrato in `uSettings.pas`
stampa esattamente i colori che `ApplyStyleToGutter` assegna, con il rapporto
di contrasto WCAG fra fondo del gutter e numeri:

| stile | fondo gutter | numeri | bordo | contrasto |
|---|---|---|---|---|
| Glow | `#242930` | `#FFFFFF` | `#191D22` | 14,64:1 |
| Windows10 Dark | `#000000` | `#FFFFFF` | `#000000` | 21,00:1 |
| Windows10 SlateGray | `#37474F` | `#FFFFFF` | `#4B5A62` | 9,65:1 |
| Windows11 Impressive Dark | `#16253D` | `#FFFFFF` | `#121F35` | 15,37:1 |
| Windows11 Modern Dark | `#2B2B2B` | `#FFFFFF` | `#1F1F1F` | 14,16:1 |
| Windows11 Polar Dark | `#343648` | `#FFFFFF` | `#2A2C3C` | 11,88:1 |
| Windows10 BlackPearl | `#212B35` | `#FFFFFF` | `#444E59` | 14,37:1 |
| Windows10 Blue Whale | `#22334C` | `#E1E1E1` | `#202F46` | 9,76:1 |
| Windows | `#F0F0F0` | `#000000` | `#FFFFFF` | 18,43:1 |
| Windows10 | `#F3F3F3` | `#000000` | `#FFFFFF` | 18,93:1 |
| Windows11 Modern Light | `#F3F3F3` | `#000000` | `#FFFFFF` | 18,93:1 |
| Flat UI Light | `#FFFFFF` | `#000000` | `#FFFFFF` | 21,00:1 |
| **colori grezzi (comportamento precedente)** | **`#F0F0F0`** | **`#000000`** | `#FFFFFF` | 18,43:1 |

Tutti e otto gli stili scuri danno un gutter scuro con numeri chiari, con
contrasto fra 9,65:1 e 21:1 — sempre oltre la soglia AA di 4,5:1. I quattro
stili chiari restituiscono gli stessi valori di prima, quindi non c'e'
regressione sul tema chiaro. L'ultima riga e' il difetto: i colori grezzi danno
`#F0F0F0` su `#000000` **qualunque** sia lo stile attivo.

### Nota

La correzione e' nell'applicazione, non in SynEdit. Renderlo style-aware a
monte sarebbe piu' elegante — il gutter potrebbe risolvere da se' i colori
lasciati al valore di default — ma vorrebbe dire modificare una dipendenza
vendored e distinguere fra "colore di default" e "colore scelto dall'utente",
distinzione che oggi non esiste perche' `TColor` non ha un valore "non
impostato" per questi campi.
