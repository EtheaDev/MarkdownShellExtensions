Markdown support test
=====================

![Markdown logo](markdownlogo.png)

This page is a small demonstration of Markdown support.

The file is encoded in UTF-8 format with BOM (this is a UTF-8 symbol: €)

<https://www.markdownguide.org>

Referenced link: From [CommonMark]:
>Markdown is a plain text format for writing structured documents,
>based on conventions for indicating formatting in email and Usenet posts.
>It was developed by John Gruber (with help from Aaron Swartz)
>and released in 2004 in the form of a syntax description and a
>Perl script (Markdown.pl) for converting Markdown to HTML.
>In the next decade, dozens of implementations were developed in many languages. 

[CommonMark]:http://spec.commonmark.org/0.28/

# heading 1
## heading 2
### heading 3

*Italic* or _Italic_

**Bold** or __Bold__  

`<Code SPAN>`
  
~~Strike~~

++Underline++

Subscript <sub>text</sub>

Superscript <sup>text</sup>

==Mark==  

```Delphi
procedure HelloWorld;
begin
  ShowMessage('Hello World');
end;
```
* Unordered List one
* Unordered List two
* Unordered List three

1. Ordered List one
1. Ordered List two
1. Ordered List three

> Block quote

---
### Horizontal rule

## Math formulas

Inline formula written between single dollar signs: $E = mc^2$ rendered inside the text.

Use double dollar signs for a centered formula block:

$$\frac{-b \pm \sqrt{b^2 - 4ac}}{2a}$$

## Tables

| First Header | Second Header | Third Header |
| :----------- | :-----------: | -----------: |
| Left         | Center        | Right        |
| Second row   | **strong**    | *italic*     |

### Evolved table (mixed inline formatting)

Each cell is an independent inline scope: inline markers (`~`, `**`, `` ` ``,
`~~`, ...) must NOT span across cells/rows.

| Header **A** | Header *B* | Col `C` |
| :----------- | :--------: | ------: |
| **strong**          | *italic*              | `code()`           |
| ~~strike~~          | [link](http://x.it)   | a~b~c              |
| H~2~O               | x^2^                  | plain text         |
| pipe \| escaped     | normal                | end                |
| **Totale LDV/anno** | **~7.000 – 8.000**    | ~30-35 LDV/giorno  |
