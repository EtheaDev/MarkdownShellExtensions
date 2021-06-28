object dmThumbnailResources: TdmThumbnailResources
  OldCreateOrder = False
  Height = 260
  Width = 341
  object DefaultTemplate: TXMLDocument
    NodeIndentStr = #9
    Options = [doNodeAutoIndent]
    XML.Strings = (
      '<?xml version="1.0"?>'
      
        '<xsl:stylesheet version="1.1" xmlns:xsl="http://www.w3.org/1999/' +
        'XSL/Transform" xmlns:b="http://www.fatturapa.gov.it/sdi/fatturap' +
        'a/v1.1" xmlns:c="http://www.fatturapa.gov.it/sdi/fatturapa/v1.0"' +
        ' xmlns:a="http://ivaservizi.agenziaentrate.gov.it/docs/xsd/fattu' +
        're/v1.2" xmlns:d="http://ivaservizi.agenziaentrate.gov.it/docs/x' +
        'sd/fatture/v1.0">'
      '  <xsl:output method="html"/>'
      
        '  <xsl:decimal-format name="euro" decimal-separator="," grouping' +
        '-separator="."/>'
      '  <xsl:template name="FormatDateIta">'
      '    <xsl:param name="DateTime"/>'
      
        '    <xsl:variable name="year" select="substring($DateTime,1,4)"/' +
        '>'
      
        '    <xsl:variable name="month" select="substring($DateTime,6,2)"' +
        '/>'
      '    <xsl:variable name="day" select="substring($DateTime,9,2)"/>'
      '    <xsl:value-of select="$day"/>'
      '    <xsl:value-of select="'#39'-'#39'"/>'
      '    <xsl:value-of select="$month"/>'
      '    <xsl:value-of select="'#39'-'#39'"/>'
      '    <xsl:value-of select="$year"/>'
      '  </xsl:template>'
      '  <xsl:template name="FormatIVA">'
      '    <xsl:param name="Natura"/>'
      '    <xsl:param name="IVA"/>'
      '    <xsl:choose>'
      '      <xsl:when test="$Natura">'
      '        <xsl:value-of select="$Natura"/>'
      '      </xsl:when>'
      '      <xsl:otherwise>'
      '        <xsl:if test="$IVA">'
      
        '          <xsl:value-of select="format-number($IVA,  '#39'###.###.##' +
        '0,00'#39', '#39'euro'#39')"/>'
      '        </xsl:if>'
      '      </xsl:otherwise>'
      '    </xsl:choose>'
      '  </xsl:template>'
      '  <xsl:template name="FatturaElettronica">'
      '    <xsl:param name="TipoFattura"/>'
      '    <xsl:param name="IsFPRS"/>'
      '    <xsl:param name="CessionarioCommittente"/>'
      '    <xsl:param name="CedentePrestatore"/>'
      '    <xsl:variable name="TD">'
      
        '      <xsl:value-of select="$TipoFattura/FatturaElettronicaBody[' +
        '1]/DatiGenerali/DatiGeneraliDocumento/TipoDocumento"/>'
      '    </xsl:variable>'
      '    <xsl:choose>'
      '      <!-- Colore per Fattura:  blu scuro-->'
      '      <xsl:when test="$TD='#39'TD01'#39' or $TD='#39'TD02'#39' or $TD='#39'TD07'#39'">'
      
        '        <path fill="darkblue" d="m15 5 145 1.9e-6 45 45v145c0 5.' +
        '54-4.46 10-10 10h-180c-5.54 0-10-4.46-10-10v-180c0-5.54 4.46-10 ' +
        '10-10z"/>'
      '      </xsl:when>'
      '      <!-- Colore per Fattura Differita: verde scuro-->'
      '      <xsl:when test="$TD='#39'TD24'#39' or $TD='#39'TD25'#39'">'
      
        '        <path fill="darkgreen" d="m15 5 145 1.9e-6 45 45v145c0 5' +
        '.54-4.46 10-10 10h-180c-5.54 0-10-4.46-10-10v-180c0-5.54 4.46-10' +
        ' 10-10z"/>'
      '      </xsl:when>'
      '      <!-- Colore per tutti gli altri tipi: rosso scuro -->'
      '      <xsl:otherwise>'
      
        '        <path fill="darkred" d="m15 5 145 1.9e-6 45 45v145c0 5.5' +
        '4-4.46 10-10 10h-180c-5.54 0-10-4.46-10-10v-180c0-5.54 4.46-10 1' +
        '0-10z"/>'
      '      </xsl:otherwise>'
      '    </xsl:choose>'
      
        '    <path d="m205 50c-12.913-0.0125-22.087 0.03402-35 0-8.1511-0' +
        '.06177-10.038-5.8522-10-10v-35z" fill="#b1d1e0"/>'
      
        '    <text font-family="Segoe UI" font-size="22" style="fill:whit' +
        'e">'
      '      <tspan x="13" y="40" font-size="32" font-weight="bold">'
      '        <!--TIPO FATTURA-->'
      '        <xsl:choose>'
      '          <xsl:when test="$TD='#39'TD01'#39'">Fattura</xsl:when>'
      '          <xsl:when test="$TD='#39'TD02'#39'">Acc/Fatt.</xsl:when>'
      '          <xsl:when test="$TD='#39'TD03'#39'">Acc/Parc.</xsl:when>'
      '          <xsl:when test="$TD='#39'TD04'#39'">N.credito</xsl:when>'
      '          <xsl:when test="$TD='#39'TD05'#39'">N.debito</xsl:when>'
      '          <xsl:when test="$TD='#39'TD06'#39'">Parcella</xsl:when>'
      '          <xsl:when test="$TD='#39'TD16'#39'">Rev.charge</xsl:when>'
      '          <xsl:when test="$TD='#39'TD17'#39'">Autof/estero</xsl:when>'
      '          <xsl:when test="$TD='#39'TD18'#39'">Fatt.CEE</xsl:when>'
      '          <xsl:when test="$TD='#39'TD19'#39'">Ex art.17</xsl:when>'
      '          <xsl:when test="$TD='#39'TD20'#39'">AutoFattura</xsl:when>'
      '          <xsl:when test="$TD='#39'TD21'#39'">AutoFattura</xsl:when>'
      '          <xsl:when test="$TD='#39'TD22'#39'">Beni Depo.IVA</xsl:when>'
      '          <xsl:when test="$TD='#39'TD23'#39'">Beni Depo.IVA</xsl:when>'
      '          <xsl:when test="$TD='#39'TD24'#39'">Fatt.Diff.</xsl:when>'
      '          <xsl:when test="$TD='#39'TD25'#39'">Fatt.Diff.</xsl:when>'
      '          <xsl:when test="$TD='#39'TD26'#39'">Cess.Beni</xsl:when>'
      '          <xsl:when test="$TD='#39'TD27'#39'">Autoconsumo</xsl:when>'
      '          <!--FPRS-->'
      '          <xsl:when test="$TD='#39'TD07'#39'">Fattura</xsl:when>'
      '          <xsl:when test="$TD='#39'TD08'#39'">N.credito</xsl:when>'
      '          <xsl:when test="$TD='#39'TD09'#39'">N.debito</xsl:when>'
      '          <xsl:otherwise>*Error*</xsl:otherwise>'
      '        </xsl:choose>'
      '      </tspan>'
      '      <tspan x="13" y="70">'
      '        <!--NUMERO FATTURA-->'
      
        '        <xsl:value-of select="$TipoFattura/FatturaElettronicaBod' +
        'y[1]/DatiGenerali/DatiGeneraliDocumento/Numero"/>'
      '      </tspan>'
      '      <tspan x="13" y="105" font-size="32" font-weight="bold">'
      '        <!--DATA FATTURA-->'
      '        <xsl:call-template name="FormatDateIta">'
      
        '          <xsl:with-param name="DateTime" select="$TipoFattura/F' +
        'atturaElettronicaBody[1]/DatiGenerali/DatiGeneraliDocumento/Data' +
        '"/>'
      '        </xsl:call-template>'
      '      </tspan>'
      '      <tspan x="13" y="130">'
      '        <!--FORNITORE-->'
      '        <xsl:value-of select="$CedentePrestatore"/>'
      '      </tspan>'
      '      <tspan x="13" y="160">'
      '        <!--CLIENTE-->'
      '        <xsl:value-of select="$CessionarioCommittente"/>'
      '      </tspan>'
      '      <tspan x="13" y="195" font-size="32" font-weight="bold">'
      '        <xsl:choose>'
      
        '          <!--IMPORTO Fattura elettronica semplificata: somma Da' +
        'tiBeniServizi/Importo-->'
      '          <xsl:when test="$IsFPRS='#39'1'#39'">'
      
        '            <xsl:for-each select="$TipoFattura/FatturaElettronic' +
        'aBody[1]/DatiBeniServizi">'
      
        '              <xsl:value-of select="format-number(sum(Importo), ' +
        #39#8364' ###.###.##0,00'#39', '#39'euro'#39')"/>'
      '            </xsl:for-each>'
      '          </xsl:when>'
      
        '          <!--IMPORTO Fattura elettronica: se esiste lo legge da' +
        ' DatiGenerali/DatiGeneraliDocumento/ImportoTotaleDocumento-->'
      
        '          <xsl:when test="$TipoFattura/FatturaElettronicaBody[1]' +
        '/DatiGenerali/DatiGeneraliDocumento/ImportoTotaleDocumento">'
      
        '            <xsl:value-of select="format-number($TipoFattura/Fat' +
        'turaElettronicaBody[1]/DatiGenerali/DatiGeneraliDocumento/Import' +
        'oTotaleDocumento, '#39#8364' ###.###.##0,00'#39', '#39'euro'#39')"/>'
      '          </xsl:when>'
      '          <xsl:otherwise>'
      
        '            <!--IMPORTO Fattura elettronica: se non esiste somma' +
        ' DatiPagamento/DettaglioPagamento/ImportoPagamento-->'
      
        '            <xsl:for-each select="$TipoFattura/FatturaElettronic' +
        'aBody[1]/DatiPagamento">'
      '              <xsl:for-each select="DettaglioPagamento">'
      
        '                <xsl:value-of select="format-number(sum(ImportoP' +
        'agamento), '#39#8364' ###.###.##0,00'#39', '#39'euro'#39')"/>'
      '              </xsl:for-each>'
      '            </xsl:for-each>'
      '          </xsl:otherwise>'
      '        </xsl:choose>'
      '      </tspan>'
      '    </text>'
      
        '    <path style="opacity:0.2;fill:#ffffff" d="1.115 -2.5,2.5 V 1' +
        '0.5 C 12,9.1149999 13.115,7.9999999 14.5,7.9999999 H 37 c 0,-1 0' +
        ',0 0,-1 z"/>'
      '  </xsl:template>'
      '  <!--inizio rendering -->'
      '  <xsl:template match="/">'
      
        '    <svg width="210mm" height="210mm" version="1.1" viewBox="0 0' +
        ' 210 210" xmlns="http://www.w3.org/2000/svg">'
      '      <xsl:choose>'
      '        <xsl:when test="d:FatturaElettronicaSemplificata">'
      '          <!--versione 1.0 SEMPLIFICATA-->'
      '          <xsl:call-template name="FatturaElettronica">'
      
        '            <xsl:with-param name="TipoFattura" select="d:Fattura' +
        'ElettronicaSemplificata"/>'
      '            <xsl:with-param name="IsFPRS" select="1"/>'
      '            <!--Cessionario Committente (cliente)-->'
      
        '            <xsl:with-param name="CessionarioCommittente" select' +
        '="d:FatturaElettronicaSemplificata/FatturaElettronicaHeader/Cess' +
        'ionarioCommittente/AltriDatiIdentificativi/Denominazione"/>'
      '            <!--Cedente/Prestatore (fornitore)-->'
      
        '            <xsl:with-param name="CedentePrestatore" select="d:F' +
        'atturaElettronicaSemplificata/FatturaElettronicaHeader/CedentePr' +
        'estatore/Denominazione"/>'
      '          </xsl:call-template>'
      '        </xsl:when>'
      '        <xsl:when test="c:FatturaElettronica">'
      '          <!--versione 1.0-->'
      '          <xsl:call-template name="FatturaElettronica">'
      
        '            <xsl:with-param name="TipoFattura" select="c:Fattura' +
        'Elettronica"/>'
      '            <xsl:with-param name="IsFPRS" select="0"/>'
      '            <!--Cessionario Committente (cliente)-->'
      
        '            <xsl:with-param name="CessionarioCommittente" select' +
        '="c:FatturaElettronica/FatturaElettronicaHeader/CessionarioCommi' +
        'ttente/DatiAnagrafici[1]/Anagrafica/Denominazione"/>'
      '            <!--Cedente/Prestatore (fornitore)-->'
      
        '            <xsl:with-param name="CedentePrestatore" select="c:F' +
        'atturaElettronica/FatturaElettronicaHeader/CedentePrestatore/Dat' +
        'iAnagrafici[1]/Anagrafica/Denominazione"/>'
      '          </xsl:call-template>'
      '        </xsl:when>'
      '        <xsl:when test="b:FatturaElettronica">'
      '          <!--versione 1.1-->'
      '          <xsl:call-template name="FatturaElettronica">'
      
        '            <xsl:with-param name="TipoFattura" select="b:Fattura' +
        'Elettronica"/>'
      '            <xsl:with-param name="IsFPRS" select="0"/>'
      '            <!--Cessionario Committente (cliente)-->'
      
        '            <xsl:with-param name="CessionarioCommittente" select' +
        '="b:FatturaElettronica/FatturaElettronicaHeader/CessionarioCommi' +
        'ttente/DatiAnagrafici[1]/Anagrafica/Denominazione"/>'
      '            <!--Cedente/Prestatore (fornitore)-->'
      
        '            <xsl:with-param name="CedentePrestatore" select="b:F' +
        'atturaElettronica/FatturaElettronicaHeader/CedentePrestatore/Dat' +
        'iAnagrafici[1]/Anagrafica/Denominazione"/>'
      '          </xsl:call-template>'
      '        </xsl:when>'
      '        <xsl:otherwise>'
      '          <xsl:call-template name="FatturaElettronica">'
      '            <!--versione 1.2-->'
      
        '            <xsl:with-param name="TipoFattura" select="a:Fattura' +
        'Elettronica"/>'
      '            <xsl:with-param name="IsFPRS" select="0"/>'
      '            <!--Cessionario Committente (cliente)-->'
      
        '            <xsl:with-param name="CessionarioCommittente" select' +
        '="a:FatturaElettronica/FatturaElettronicaHeader/CessionarioCommi' +
        'ttente/DatiAnagrafici[1]/Anagrafica/Denominazione"/>'
      '            <!--Cedente/Prestatore (fornitore)-->'
      
        '            <xsl:with-param name="CedentePrestatore" select="a:F' +
        'atturaElettronica/FatturaElettronicaHeader/CedentePrestatore/Dat' +
        'iAnagrafici[1]/Anagrafica/Denominazione"/>'
      '          </xsl:call-template>'
      '        </xsl:otherwise>'
      '      </xsl:choose>'
      '    </svg>'
      '  </xsl:template>'
      '</xsl:stylesheet>'
      '')
    Left = 43
    Top = 78
    DOMVendorDesc = 'MSXML'
  end
  object SourceXML: TXMLDocument
    Left = 48
    Top = 16
  end
  object EditingTemplate: TXMLDocument
    NodeIndentStr = #9
    Options = [doNodeAutoIndent]
    Left = 43
    Top = 142
    DOMVendorDesc = 'MSXML'
  end
end
