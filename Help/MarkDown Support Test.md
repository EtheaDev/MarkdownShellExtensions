Markdown support test
=====================

![Markdown logo](markdownlogo.png)

This page is a small demonstration of Markdown support.

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

## Tables

| First Header | Second Header | Third Header |
| :----------- | :-----------: | -----------: |
| Left         | Center        | Right        |
| Second row   | **strong**    | *italic*     |

## Formulas

It is possible to use the [Google Chart API] using `TeX` language,
but the translated formula can only to be seen using a browser,
for insert a formula using `TeX` enclose the code between `$`
without `spaces`: 

Quadratic formula          |Zeta formula
---------------------------|-----------------------------
$x=\frac{-b\pm\sqrt{b^2-4ac}}{2a}$ | $\zeta(s)=\sum_{n=1}^\infty\frac{1}{n^s}$

[Google Chart API]:https://developers.google.com/chart/infographics/docs/formulas
