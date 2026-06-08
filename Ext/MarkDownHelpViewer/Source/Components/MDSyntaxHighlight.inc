{******************************************************************************}
{                                                                              }
{       MarkDown Shell extensions - Syntax Highlighting switch                 }
{                                                                              }
{       Include this file and toggle the define below to enable/disable the    }
{       syntax highlighting of fenced code blocks in the HTML preview.         }
{                                                                              }
{       When MD_SYNTAX_HIGHLIGHTING is DEFINED:                                }
{         - fenced code blocks (```delphi, ```json, ...) are colorized using   }
{           the SynEdit highlighter suite + TSynExporterHTML;                  }
{         - the whole SynEdit highlighter suite is linked into the binary.     }
{                                                                              }
{       When MD_SYNTAX_HIGHLIGHTING is NOT defined:                            }
{         - CreateCodeHighlightEmitter returns nil and the MarkdownProcessor   }
{           falls back to its default <pre><code> emission;                    }
{         - NO SynEdit unit is referenced, so a documentation-only viewer      }
{           (that never shows code) does not carry the SynEdit dependency.     }
{                                                                              }
{       To build WITHOUT syntax highlighting, comment out the line below.      }
{                                                                              }
{******************************************************************************}

{$DEFINE MD_SYNTAX_HIGHLIGHTING}
