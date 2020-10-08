{*************************************************************************}
{ TMS TAdvRichEditor                                                      }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2014 - 2019                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvRichEditor;

{$I TMSDEFS.INC}
{$IFDEF FMXMOBILE}
{$DEFINE DELPHI_LLVM}
{$ENDIF}

{$IFDEF VCLLIB}
{$IFDEF DELPHIXE2_LVL}
{$DEFINE USEVCLSTYLES}
{$ENDIF}
{$IFDEF FNCLIB}
{$DEFINE USEVCLSTYLES}
{$ENDIF}
{$ENDIF}
{$IFNDEF LCLLIB}
{$HINTS OFF}
{$IF COMPILERVERSION > 31}
{$DEFINE DELPHITOKYO}
{$IFEND}
{$HINTS ON}
{$ENDIF}

interface

uses
  Classes, Types, Generics.Collections,
  AdvRichEditorPic, AdvRichEditorBase, AdvRichEditorRTF,
  AdvGraphicsTypes, AdvUtils, AdvScrollControl,{%H-}AdvToolBarRes
  {$IFDEF VCLLIB}
  , Windows, Messages, Clipbrd
  , ImgList, Printers, ActnList
  , Graphics, Controls, Dialogs, Forms, ExtCtrls, Menus
  , JPEG, GIFImg, PngImage
  , AdvRichEditorDD
  {$ENDIF}
  {$IFDEF FMXLIB}
  , FMX.Graphics, FMX.Controls, FMX.Dialogs, FMX.Forms, FMX.ExtCtrls
  , FMX.Menus, FMX.Types, System.Math.Vectors, System.Actions, FMX.ActnList
  {$IFNDEF FNCLIB}
  , FMX.TMSTypes
  {$ENDIF}
  {$ENDIF}
  {$IFDEF DELPHIXE_LVL}
  , AdvPicture, GDIPicture, AdvGDIP
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  {$IFNDEF LCLLIB}
  , System.UITypes
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF FNCLIB}
  {$IFDEF DELPHIXE3_LVL}
  , AnsiStrings
  {$ENDIF}
  {$ENDIF}
  {$IFDEF TMSPACK}
  , GDIPPictureContainer
  {$ENDIF}
  {$IFDEF TMSDEBUG}
  , TMSLogging
  {$ENDIF}
  {$IFDEF FNCLIB}
  , AdvTypes, AdvToolBarPopup, AdvPopup
  {$ENDIF}
  {$IFDEF LCLLIB}
  {$IFDEF MSWINDOWS}
  , Windows
  {$ENDIF}
  , LCLType, LMessages, LCLIntf
  , Clipbrd, Graphics, Controls, Dialogs, Forms, ExtCtrls, Menus
  , ImgList, Printers, ActnList
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 7; // Minor version nr.
  REL_VER = 3; // Release nr.
  BLD_VER = 0; // Build nr.
  DATE_VER = 'Jul, 2019'; // Month version

  TTMSFNCRichEditorDocURL = 'http://www.tmssoftware.biz/download/manuals/TMSFNCRichEditorDevGuide.pdf';
  TTMSFNCRichEditorTipsURL = 'http://www.tmssoftware.com/site/tmsfncuipack.asp?s=faq';

  // version history
  // v1.0.0.0 : First release
  // v1.0.1.0 : New : Public functions
  //                  ContentAsHTML, ContentAsRTF, SelectionAsHTML, SelectionAsRTF
  //          : Fixed : Issue with changing formatting when editor has no selection
  // v1.0.1.1 : Fixed : Issue with retrieving font settings for selected text in the editor
  // v1.0.1.2 : Fixed : Issue with setting color, background color when no text is selected
  // v1.0.2.0 : New : Method SetCaret() added
  //          : Fixed : Rare issue with bullet insertion
  //          : Improved : Unicode character export to RTF
  // v1.0.2.1 : Fixed: Issue with inserting image after bullet list in very specific circumstances
  // v1.0.3.0 : New : OnCorrectWord event added
  //          : New : URLAuto property added
  //          : New : Method ClearErrors added
  //          : New : SelStart, SelLength properties added
  //          : Improved : Font handling during HTML export
  //          : Improved : Handling large text paste
  // v1.0.3.1 : Fixed : Issue with background colors for HTML export
  // v1.1.0.0 : New : AutoCorrect
  //          : New : Emoticons support
  //          : New : Support for PictureContainer images
  //          : New : OLE drag & drop support
  //          : New : AppendFile() method added
  //          : New : Interface to spell check engine
  //          : New : DB-aware version
  //          : New : TAdvRichEditorMiniHTMLIO component
  // v1.1.0.1 : Fixed : Issue with inserting text after merge fields
  //          : Fixed : Issue with spell check dialog initialization
  // v1.1.1.0 : Improved : HTML import
  //          : New : Public property ZoomFactor
  // v1.1.1.1 : Fixed : Issue with HTML generation
  // v1.1.1.2 : Improved : HTML compliance for export
  //          : Improved : Selection of graphic elements
  // v1.2.0.0 : New : Popup mini format toolbar
  //          : New : Export of custom graphic elements to PDF
  //          : New : TAdvRichEditorMiniHTMLIO.Insert(HTMLValue) added to insert snippets of HTML in richeditor
  //          : Improved : Quality of PDF export
  //          : Improved : Behavior of UnMerge
  // v1.2.0.1 : Fixed : Issue with hiding popup toolbar in specific circumstances
  // v1.2.0.2 : Fixed : Rare issue with changing background color
  //          : Improved : Auto URL detection with pasting text from clipboard
  // v1.2.0.3 : Fixed : Rare issue with inserting text after loading file
  //          : Fixed : Issue with spell check & inserting line break at specific positions
  //          : Fixed : Issue with handling return key on modal forms
  // v1.2.1.0 : New : Support for Delphi XE8 & C++Builder XE8 Prof, Ent. Architect added
  // v1.2.1.1 : Fixed : Issue with clipboard support
  // v1.3.0.0 : New : Support for VCL Styles added
  // v1.3.0.1 : Fixed : Issue with ReplaceAll() in specific circumstances
  //          : Fixed : Issue with persisting merge fields on stream
  // v1.3.1.0 : New : Event OnCanSelectGraphic, OnCanSizeGraphic added
  //          : New : Event OnClickGraphic added
  //          : New : Support for pasting basic HTML from clipboard
  // v1.3.2.0 : New : Overload for LoadFromTextFile() with Encoding parameter
  // v1.3.3.0 : New : Enhanced URLAuto handling
  // v1.3.3.1 : Fixed : Issue with char encoding when pasting from HTML
  //          : Improved : Pasting for HTML formatted text
  // v1.3.3.2 : Fixed : Issue with Modified flag handling
  // v1.3.3.3 : Fixed : Caret positioning issue in center or right aligned text
  // v1.4.0.0 : New : RTF import
  //          : New : RTF clipboard functionality
  //          : Misc : Various smaller improvements & fixes
  // v1.4.0.1 : Fixed : Issue with toolbar font picker style
  //          : Fixed : Issue with TAdvRichEditorEditingToolbar button options
  //          : Fixed : Issue with repeated copy to clipboard in RTF format
  // v1.4.0.2 : Improved : TAdvRichEditorEditToolBar Open function now has option to open RTF file as well
  //          : Fixed: Issue with paste when caret/selection is on linebreak
  //          : Improved : RTF parser compatibility with OpenOffice & LibreOffice RTF
  // v1.4.1.0 : New : function GetContentAsRTE(Selection): string added
  //          : New : property ContentAsRTE property added
  // v1.4.1.1 : Improved : Sizing of images with aspect ratio
  //          : Fixed : Issue with getting selection attributes after RTF import
  //          : Fixed : Issue with InsertMultiLineText with linebreaks
  // v1.4.2.0 : New : Property AllowSelect added
  //          : New : Drag & drop of RTF files
  //          : Improved : Insert of multiline text
  // v1.4.3.0 : New : TAdvRichEditorIO.Load() overload added with stream parameter
  // v1.4.3.1 : Fixed : Issue with find & replace
  //          : Fixed : Issue with VCL styles & scrollbar
  //          : Fixed : Issue with VCL styles and bullet color
  // v1.4.4.0 : New : Support to export to HTML with inline images
  //          : Improved : HTML import
  // v1.4.4.1 : Fixed : Issue with ReplaceAll under specific circumstances
  //          : Fixed : Issue with image drag & drop
  // v1.4.4.2 : Fixed : RTF import issue for specific codepage settings
  // v1.4.4.3 : Fixed : Issue with RTF parsing when RTF contains tabs
  // v1.4.5.0 : New : RAD Studio 10 Seattle support
  // v1.4.5.1 : Improved : HTML Import and export support
  // v1.4.6.0 : Improved : Default font handled for HTML export
  //          : Fixed : Rare issue with HTML export with URLs
  // v1.4.6.1 : Improved : RTF font color import handling
  // v1.4.7.0 : New : GotoTextBegin, GotoTextEnd, GotoLineBegin and GotoLineEnd
  //          : Fixed : Caret issue during insert hyperlink
  // v1.4.8.0 : New : Import of JPEGBLIP images from RTF files
  //          : New : Public property .ClipboardFormats to allow to exclude specific formats from pasting
  //          : New : OnPasteText event , OnPasteFormattedText event
  //          : New : Public property BulletSpacing added
  //          : Improved RTF attributes import
  //          : Improved RTF unicode font import
  //          : Improved RTF clipboard paste
  //          : Fixed : parsing of subscript / superscript from RTF files
  // v1.4.8.1 : Fixed : Issue with RTF color table import
  // v1.4.8.2 : Fixed : Issue with backspace & tab
  // v1.4.9.0 : New : Property ContentHeight added to get the height of the content in TAdvRichEditor
  // v1.4.9.1 : Improved : Handling of bullet insert/delete
  // v1.4.9.2 : Fixed : Issue with repeatedly add & remove bullet
  //          : Fixed : Issue with selecting tabs
  // v1.4.9.3 : Fixed : Indent handling on bulleted items
  //          : Fixed : Issue with RTF Japanese char encoding
  // v1.5.0.0 : New : Large caps, small caps text style added
  // v1.5.0.1 : Fixed : Streaming issue with tabs
  //          : Fixed : Issue with bullet indenting
  // v1.5.1.0 : New : OnDrawBackground event added
  // v1.5.2.0 : New : Actions TAdvRichEditorSmallCaps, TAdvRichEditorLargeCaps
  //          : New : Font ribbon font size increase, font size decrease buttons added
  // v1.5.2.1 : Fixed : Issue with RTF bullet import
  //          : Improved : Handling left/right single/double quotes in RTF
  // v1.5.2.2 : Fixed : Issue with ReadOnly = true and drag & drop
  //          : Fixed : Particular issue with wordwrap
  // v1.5.3.0 : New : URLOpen property added
  //          : New : URLUnderline property added
  //          : Fixed : Issue with selection display in wordwrapped text
  //          : Fixed : Issue with URL handpoint display in wordwrapped text
  //          : Fixed : Issue with parsing specials chars from RTF
  //          : Improved : Mac RTF font table parsing
  //          : Improved : Outlook RTF parsing
  // v1.5.3.1 : Fixed : Issue with size calculation for indented very long words
  //          : Fixed : Handling of very large words & horiz. scroll
  // v1.5.3.2 : Fixed : Issue with cut to clipboard of pictures
  //          : Fixed : Issue with horz. line drawing & scrolling
  // v1.5.4.0 : New : Support to handle PNG images in RTF
  // v1.5.5.0 : New : function SetPageSize to automatically set size to most used paper formats
  // v1.5.5.1 : Fixed : Issue with caret display during horz. scroll
  // v1.5.5.2 : Fixed : Issue with inserting specific sequence of HTML formatted text
  // v1.5.5.3 : Fixed : Issue with HTML export of font attributes
  // v1.5.6.0 : New : RemoveSelectionBullets method added
  //          : New : SaveToText() overload that allows to specify the encoding
  //          : Improved : Scaled picture RTF parsing
  //          : Improved : HTML parsing
  //          : Fixed : Issue with exporting text background colors to HTML
  //          : Fixed : Issue with RTF background color parsing
  // v1.5.6.1 : Improved : Faster RTF parsing
  // v1.5.6.2 : Fixed : Issue with persisting default font color in RTE file
  // v1.5.6.3 : Fixed : Issue with HTML export of indented text
  // v1.5.7.0 : Improved : Cursor handling
  //          : New : Event OnImageNotFound added in TAdvRichEditorHTMLIO
  // v1.5.8.0 : Improved : Export of bullet lists to HTML
  // v1.5.8.1 : Improved : RTF parsing from Microsoft IE / Edge browser
  // v1.5.8.2 : Improved : HTML export for text background colors
  //          : Improved : HTML import for newer DOCTYPES
  // v1.5.9.0 : New : RAD Studio 10.1 Berlin support
  // v1.5.10.0: Improved : Context menu shortcut handling
  //          : Improved : HTML export
  //          : Improved : RTF Parsing
  //          : Improved : HTML Parsing
  //          : New : SetCaret(CharPos) overload method added
  //          : New : InsertLineBreak method added
  // v1.5.11.0: Improved : HTML export
  // v1.5.11.1: Fixed : Issue with exporting scaled images to RTF & HTML
  // v1.6.0.0 : New : Support to read HTML inline images
  //          : New : method SelectionToCaret added
  // v1.6.0.1 : Improved : Applying attributes to selected text
  // v1.6.0.2 : Improved : HTML image attribute parsing
  // v1.6.0.3 : Fixed : Rare issue with wordwrap
  // v1.6.0.4 : Fixed : Issue with DB updating when selection attributes change in TDBAdvRichEditor
  // v1.6.1.0 : New : IsSelectionError function added
  // v1.6.2.0 : New : InsertSelectionFromStream method added
  //          : Fixed : Issue with clipboard copy & selection
  // v1.6.3.0 : New : Accessible color pickers & symbol picker from TAdvRichEditorFormatToolBar
  // v1.6.3.1 : Improved : Bullet characters in RTF export
  // v1.6.3.2 : Fixed : Issue with flickering when used within VCL styled forms
  // v1.6.3.3 : Fixed : Issue with parsing unicode chars in RTF
  // v1.6.3.4 : Fixed : Caret behavior with Ctrl-Home, Ctrl-End
  //          : Fixed : Issue with inserting text in long paragraphs
  //          : Fixed : Issue with copy & paste across linebreaks
  //          : Fixed : Issue with selections till end of line
  //          : Fixed : Insert of bullet when caret is at linebreak
  // v1.6.3.5 : Fixed : Issue with font caching & zoom
  // v1.6.3.6 : Fixed : Issue with OnCorrectWord() event in specific cases
  // v1.6.3.7 : Fixed : Issue with RTF color table parsing in specific cases
  // v1.6.3.8 : Fixed : Issue with action handling
  // v1.6.3.9 : Improved : popup toolbar handling
  // v1.6.4.0 : New : OnMergeField event added for merging with dynamic values
  //          : New : Dynamic merge field merging with DB merge source
  // v1.6.4.1 : Fixed : Issue with parsing accented chars from MS Word RTF documents
  // v1.6.4.2 : Improved : Performance
  // v1.6.5.0 : Improved : RTF parsing
  //          : Improved : HTML parsing
  //          : New : TAdvRichEditorHTMLIO.SpaceAsNbSp: boolean property added
  // v1.6.5.1 : Improved : Picture rendering with default size
  // v1.6.5.2 : Fixed : Issue with auto-correct via spell check at end of line
  // v1.6.6.0 : New : Public property SpaceAsNbSp: boolean added
  //          : Fixed : Issue with RTF fonttable parsing
  // v1.6.7.0 : New : OnDropFile event added
  //          : Improved : BODY style HTML parsing
  //          : Fixed : Issue with default font color & exporting to HTML
  // v1.6.7.1 : Fixed : Issue with parsing RTF on non Windows OS
  // v1.6.7.2 : Fixed : Issue with parsing RTF for strikeout chars
  //          : Fixed : Issue with parsing RTF for 'ul' attribute
  // v1.6.8.0 : New : Property PlainHTML: boolean added for TAdvRichEditorHTMLIO
  //          : Fixed : Issue with AdvRichEditorRTFIO.SaveToStream()
  // v1.6.9.0 : New : TEncoding parameters in AdvRichEditorHTMLIO.Load() and AdvRichEditorMiniHTMLIO.Load() functions
  // v1.6.9.1 : Fixed : Issue with RTF import of specific Win1252 code page chars
  // v1.6.10.0: New : SpaceAsNbSp property added in TAdvRichEditorMiniHTMLIO
  //          : New : OnChange event added
  //          : Improved : Import of HTML bullet lists and handling of linebreaks
  //          : Improved : handling of insert of unicode chars in LCL
  // v1.6.10.1: Fixed : Issue with OnCorrectWord() event in specific circumstances
  //          : Fixed : Issue with PrevWord() in specific circumstances
  // v1.6.11.0: New : Property AllowFormatShortCuts added
  // v1.6.11.1: Fixed : Issue with unicode & .SaveToText()
  // v1.6.11.2: Fixed : Issue with auto correct word in specific circumstances
  // v1.6.11.3: Improved : Spell check handling
  // v1.6.11.4: Fixed : Issue with DB merging
  //          : Fixed : Issue with popup toolbar font handling
  //          : Fixed : Issue with text size calculation for printing
  // v1.6.12.0: Improved : Ribbon shortcut hint handling for richeditor ribbon toolbars
  //          : New : var parameter ReplaceImage added in event OnImageNotFound in TAdvRichEditorHTMLIO
  // v1.6.12.1: Fixed : Rare issue with integrated spell check
  // v1.6.12.2: Fixed : Issue with HTML paragraph import
  //          : Improved : RTF import
  // v1.6.13.3: Fixed : Issue with changing attributes on merge fields
  //          : Improved : Parsing of specific MS Word elements in RTF
  // v1.6.13.4: Improved : RTF parsing
  //          : Improved : HTML parsing
  // v1.6.13.5: Improved : RTF font table handling for specific Microsoft RTF extensions
  // v1.6.13.6: Fixed : Issue with linespacing for printing
  // v1.6.14.0: New : HTMLImages.igNone option added to ignore images during export to HTML
  // v1.6.15.0: New : GetMergeFields method added to retrieve list of available merge fields in the document
  //          : Improved : Triggering of OnChange event
  // v1.6.15.1: Improved : Spellcheck in specific circumstances of entering characters
  // v1.6.16.0: New : igReference image generation type added for HTML export
  //          : New : TAdvRichEditorEmailIO class to export TAdvRichEditor to Indy idMessage to send HTML email
  //          : Improved : HTML export
  // v1.6.16.1: Fixed : Issue with calling SetSelectionAttribute() and updating default font
  // v1.6.16.2: Fixed : Issue with parsing particular RTF files
  // v1.6.16.3: Fixed : Issue with RTF unicode char parsing
  // v1.6.17.0: New : Support for HTML number character notation import
  //          : Fixed : Memory leak with popup spellcheck interface
  //          : Fixed : Default font setting handling in combination with pasting non text elements from the clipboard
  // v1.6.17.1: Fixed : Issue with InsertAsRTF() and font color handling
  // v1.6.17.2: Fixed : Issue with VCL styles and popup menu
  // v1.6.18.0: New : Property LineSpacing added
  // v1.6.18.1: Improved : RTF import
  // v1.6.18.2: Improved : Parsing of hyperlinks from RTF files
  // v1.6.18.3: Improved : HTML font style parsing
  // v1.6.18.4: Fixed : Issue with RTF color import
  // v1.6.18.5: Improved : Export to HTML of character ß
  // v1.6.19.0: Improved : Resolution of exported images to RTF
  // v1.6.19.1: Fixed : Issue with OnChange event and pressing Enter
  // v1.6.19.2: Fixed : Issue with £ char parsing in RTF
  // v1.6.19.3: Fixed : Issue with InsertMultiLineText after changing default properties
  //          : Fixed : Issue with caret update under specific circumstances when inserting text via clipboard paste
  // v1.6.19.4: Improved : Image resizing
  //          : Fixed : Issue with Find() function
  // v1.6.19.5: Fixed : Insert char after programmatically set caret
  //          : Fixed : Rare issue with ctrl-shift-select
  // v1.6.19.6: Fixed : Issue with editing in combination with spell check
  // v1.6.19.7: Fixed : Issue with find & replace
  //          : Improved : HTML export
  //          : Fixed : Issue with paste & font handling
  //          : Fixed : Issue with OnChange event handler for shortcuts to change attributes
  // v1.6.19.8: Fixed : Issue with changing keys via OnKeyDown event
  //          : Fixed : Issue with handling Ctrl-INS
  // v1.6.19.9: Fixed : Issue with RTF parsing
  //          : Fixed : Issue with importing HTML files with inline images
  // v1.6.19.10: Fixed : Issue with HTML import and font handling
  // v1.6.19.11: Fixed : Issue with parsing \ in RTF
  // v1.6.19.12: Fixed : Issue with InsertAsRTF()
  // v1.6.19.13: Fixed : Issue with line spacing during printing
  // v1.6.19.14: Fixed : Issue with image drawing
  // v1.6.19.15: Fixed : Issue with unicode char parsing from RTF files
  // v1.6.20.0 : New : Method  PaintTo(Canvas, Rect) added
  //           : New : Support for auto-correct text with text containing linebreaks
  // v1.6.20.1 : Fixed : Issue with pasting from RTF & special characters
  // v1.6.20.2 : Fixed : Issue with OnSelectionChanged() not always triggered when needed
  // v1.6.20.3 : Fixed : Issue with export of subscript / superscript to RTF
  // v1.6.20.4 : Improved : Check to avoid incorrect string access in GetCharPos()
  // v1.6.20.5 : Fixed : Issue with printing images
  // v1.6.20.6 : Improved : Handling of Unicode chars in save/load RTF functions
  // v1.6.20.7 : Improved : Small improvement with respect to caret positioning
  // v1.6.20.8 : Fixed : Issue with handling single/double quotes & spell check
  // v1.6.20.9 : Fixed : Issue with using LoadFromStream() with TDBAdvRichEditor
  // v1.6.20.10: Fixed : Issue with deleting emoticons
  //           : Fixed : Issue with deleting error underlined text in specific circumstances
  // v1.6.20.11: Fixed : Issue with TDBAdvRichEditor in specific circumstances
  //           : Fixed : Issue with manual drag & drop
  // v1.7.0.0  : New : Add Ignore option to the TAdvRichEditor context menu for spell check
  //           : New : ScrollToBegin, ScrollToEnd methods added
  // v1.7.0.1  : Fixed : Interaction with TAdvRichEditorFormatToolBar in specific cases at start of new line
  // v1.7.0.2  : Fixed : Issue with mouse cursor on mouse-leave / mouse-enter
  // v1.7.0.3  : Fixed : Rare issue with - sign used in spell-checked words
  // v1.7.0.4  : Fixed : Issue with HTML parsing where font.size is set as -1
  // v1.7.0.5  : Fixed : Issue with HTML rendering of SPAN
  // v1.7.0.6  : Improved : HTML export
  //           : Improved : TAdvRichEditorFormatToolBar handling of font size up/down keys
  //           : Improved : HTML import
  // v1.7.1.0  : New : Event OnGetElementHint added
  //           : New : Function LineCount added
  //           : New : TAdvRichEditorHTMLIO.InsertHTML() function added
  //           : Improved : Image resizer mouse sensitivity
  // v1.7.1.1  : Fixed : Issue with Clear handling
  //           : Fixed : Issue with OnClickHyperlink handling
  // v1.7.1.2  : Improved : InsertMultiLineText performance
  // v1.7.1.3  : Fixed : Issue with HTML email compatibility with single quote
  // v1.7.1.4  : Fixed : Issue with accented characters for RTF export
  // v1.7.1.5  : Fixed : Issue with importing HTML in themed applications
  // v1.7.1.6  : Fixed : Issue with DeleteSelection and OnChange event
  //           : Fixed : Issue with TAdvRichEditorMiniHTMLIO.Insert & clipboard
  // v1.7.1.7  : Fixed : Issue with default font color export to HTML
  // v1.7.1.8  : Fixed : Issue with emoticon handling
  //           : Fixed : Issue with InsertLineBreak
  // v1.7.1.9  : Fixed : Issue with font export for HTML
  // v1.7.2.0  : New : STRIKE HTML tag support added
  // v1.7.2.1  : Fixed : Issue with auto URL handling after paste
  //           : Fixed : Issue with drag caret drawing after blocked drag
  //           : Improved : Bullet drawing position
  // v1.7.3.0  : New : OnPasteBitmap event added

type
{$IFNDEF FMXLIB}
  {$EXTERNALSYM THintInfo}
  THintInfo = Controls.THintInfo;
  {$EXTERNALSYM PHintInfo}
  PHintInfo = Controls.PHintInfo;
  TActionClass = class of TAction;
{$ENDIF}

{$IFDEF FMXLIB}
  TColor = TAlphaColor;
{$ENDIF}

  TAdvRichEditor = class;

  ITMSRichEditorToolBar = interface(IInterface)
    ['{91549009-2F25-418F-9BD5-32D16A82C31B}']
    procedure UpdateToolBar;
  end;

  TRichEditorDrawGraphicEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AID: string) of object;

  TRichEditorDrawBackgroundEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect) of object;

  THyperlinkClickEvent = procedure(Sender: TObject; URL: string) of object;

  TGetElementHintEvent = procedure(Sender: TObject; AElement: TREElement; var AHint: string) of object;

  TGraphicClickEvent = procedure(Sender: TObject; AGraphic: TREElement) of object;

  TGraphicAllowEvent = procedure(Sender: TObject; AGraphic: TREElement; var Allow: boolean) of object;

  TWordContextEvent = procedure(Sender: TObject; MousePos: TPoint; AWord: string; AElement: TREElement; var Handled: boolean) of object;

  TPasteTextEvent = procedure(Sender: TObject; var AText: string; var Allow: boolean) of object;

  TPasteBitmapEvent = procedure(Sender: TObject; ABitmap: TBitmap; var Allow: boolean) of object;

  TPasteFormattedTextEvent = procedure(Sender: TObject; AStream: TStream) of object;

  TDropFileEvent = procedure(Sender: TObject; AFileName: string; var Allow: boolean) of object;

  TDropImageEvent = procedure(Sender: TObject; AImage: TPicture; var Allow: boolean) of object;

  TImageResizeCorner = (irTopLeft,irTopRight,irBottomLeft,irBottomRight);

  TImageNotFoundEvent = procedure(Sender: TObject; ImageName: string; var replaceimage: string) of object;

  TGraphicSelectionStyle = (gsRect, gsCircle);

  TClipboardFormat = (cfRTE, cfRTF, cfText, cfHTML, cfBMP, cfFile);

  TPopupMenuAction = (pmClear, pmCut, pmCopy, pmPaste, pmAlignLeft, pmAlignCenter, pmAlignRight, pmNone);

  TClipboardFormats = set of TClipboardFormat;

  {$IFNDEF TMSPACK}
  TGDIPPictureContainer = class(TComponent);
  {$ENDIF}

  TGraphicSelection = class(TPersistent)
  private
    FBorderColor: TColor;
    FColor: TColor;
    FStyle: TGraphicSelectionStyle;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Style: TGraphicSelectionStyle read FStyle write FStyle default gsRect;
    property Color: TColor read FColor write FColor default clWhite;
    property BorderColor: TColor read FBorderColor write FBorderColor default clGray;
  end;

  {$IFNDEF FNCLIB}
  TAdvRichEditorDropTarget = class(TRichEditorDropTarget)
  private
    FRichEditor: TAdvRichEditor;
  public
    constructor Create(AEditor: TAdvRichEditor);
    procedure DropText(pt: TPoint;s: string; dwEffect: longint); override;
    procedure DropStream(pt: TPoint; AStream: TMemoryStream; dwEffect: longint); override;
    procedure DropRTF(pt: TPoint;s: string; dwEffect: longint); override;
    procedure DropFiles(pt: TPoint;Files: TStrings; dwEffect: longint); override;
    procedure DropURL(pt: TPoint; s: string; dwEffect: longint); override;
    procedure DropBMP(pt: TPoint; bmp: Graphics.TBitmap; dwEffect: longint); override;
    procedure DragMouseMove(pt:TPoint; var Allow: Boolean; DropFormats: TDropFormats); override;
    procedure DragMouseLeave; override;
  end;

  TAdvRichEditorDropSource = class(TRichEditorDropSource)
  private
    FRichEditor: TAdvRichEditor;
    FLastEffect: Integer;
  protected
    procedure DragDropStop; override;
  public
    constructor Create(AEditor: TAdvRichEditor);
    procedure CurrentEffect(dwEffect: Longint); override;
    procedure QueryDrag; override;
    property LastEffect: Integer read FLastEffect;
  end;
  {$ENDIF}

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvRichEditorPopup = class(TComponent)
  private
    FRichEditor: TAdvRichEditor;
    FVisible: boolean;
  protected
    property RichEditor: TAdvRichEditor read FRichEditor write FRichEditor;
  public
    procedure Show({%H-}PT: TPoint); virtual;
    procedure Hide; virtual;
    function MouseInPopup(PT: TPoint): boolean; virtual; abstract;
    property Visible: boolean read FVisible;
  end;

  {$IFDEF FNCLIB}
  TTMSFNCRichEditorCustomToolBarPopup = class(TTMSFNCCustomToolBarPopup)
  private
    FRichEditor: TTMSFNCRichEditor;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    property RichEditor: TTMSFNCRichEditor read FRichEditor write FRichEditor;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  {$ENDIF}


  TToolBarList = TList<ITMSRichEditorToolBar>;

  {$IFDEF FNCLIB}
  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatforms)]
  {$ENDIF}
  {$ENDIF}
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  {$IFDEF FNCLIB}
  TAdvRichEditor = class(TAdvRichEditorBase, ITMSFNCCustomEditor)
  {$ENDIF}
  {$IFNDEF FNCLIB}
  TAdvRichEditor = class(TAdvRichEditorBase)
  {$ENDIF}
  private
  {$IFNDEF FNCLIB}
    FDropTarget: TAdvRichEditorDropTarget;
    FDropSource: TAdvRichEditorDropSource;
  {$ENDIF}
  {$IFDEF FNCLIB}
    FToolBarPopup: TTMSFNCRichEditorCustomToolBarPopup;
  {$ENDIF}
  {$IFDEF FMXLIB}
    FImeLine: Integer;
    FImeStartIndex: Integer;
    FImeString: string;
  {$ENDIF}
    FToolBarList: TToolBarList;
  {$IFDEF TMSPACK}
    FGDIPPicture: TGDIPPicture;
    FGDIPPictureContainer: TGDIPPictureContainer;
    FEmoticons: TGDIPPictureContainer;
  {$ENDIF}
    FDropTargetAssigned: boolean;
    FMouseDown: boolean;
    FDownPos: TPoint;
    FDownXY: TPoint;
    FDownSize: TSize;
    FCaretTimer: TTimer;
    FDoCaret: boolean;
    FDoImageResize: boolean;
    FImageResizeCorner: TImageResizeCorner;
    FPopupMenu: TPopupMenu;
  {$IFNDEF FMXLIB}
    FImageList: TImageList;
  {$ENDIF}
    FOnDrawGraphic: TRichEditorDrawGraphicEvent;
    FOnDrawBackground: TRichEditorDrawBackgroundEvent;
    FOnClickHyperlink: THyperlinkClickEvent;
    FOnClickGraphic: TGraphicClickEvent;
    FOnContextForWord: TWordContextEvent;
    FOnCanSelectGraphic: TGraphicAllowEvent;
    FOnCanSizeGraphic: TGraphicAllowEvent;
    FClearAction: TAdvRichEditorClear;
    FCutAction: TAdvRichEditorCut;
    FCopyAction: TAdvRichEditorCopy;
    FPasteAction: TAdvRichEditorPaste;
    FLeftAction: TAdvRichEditorAlignLeft;
    FCenterAction: TAdvRichEditorAlignCenter;
    FRightAction: TAdvRichEditorAlignRight;
    FGraphicSelection: TGraphicSelection;
    FLastHint: string;
    FClickOnSel: boolean;
    FClickSelXY: TPoint;
    FInternalDD: boolean;
    FZoomFactor: double;
    FSingleLine: boolean;
    FRichEditorPopup: TAdvRichEditorPopup;
    FAllowSelect: boolean;
    FClipboardFormats: TClipboardFormats;
    FOnPasteText: TPasteTextEvent;
    FOnPasteBitmap: TPasteBitmapEvent;
    FOnPasteFormattedText: TPasteFormattedTextEvent;
    FOnImageNotFound: TImageNotFoundEvent;
    FOnDropFile: TDropFileEvent;
    FOnDropImage: TDropImageEvent;
    FOnGetElementHint: TGetElementHintEvent;
    FCacheFont: TFont;
    FCacheHeight: integer;
    FOrigCursor: TCursor;
    FOrigIBeam: boolean;
    {$IFNDEF FNCLIB}
    FIsPrinting: boolean;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FIsPainting: boolean;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    {$IFDEF USEVCLSTYLES}
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VCLLIB}
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    {$ENDIF}
    {$IFDEF LCLLIB}
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message LM_GETDLGCODE;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message LM_LBUTTONDBLCLK;
    procedure WMDestroy(var Message: TWMDestroy); message LM_DESTROY;
    {$ENDIF}
    procedure SetVersion(const {%H-}Value: string);
    function GetVersionNr: Integer;
    procedure SetGraphicSelection(const Value: TGraphicSelection);
    procedure SetZoomFactor(const Value: double);
  protected
    function GetVersion: string; override;
    function GetDocURL: string; override;
    function GetTipsURL: string; override;
    function GetTextDescent(ACanvas: TCanvas; {%H-}el: TREElement): integer; override;
    function GetTextSize(ACanvas: TCanvas; {%H-}el: TREElement; AValue: string): TTextSize; override;
    procedure UpdateBulletFont({%H-}ACanvas: TCanvas);
    function GetBulletSize(el: TREElement; AValue: string): TSize; override;
    function GetPictureSize(el: TREElement): TSize; override;
    function GetCharInWord(el: TREElement; s: string; X: integer): integer; override;
    function GetCharPos(AValue: string; CharIndex: integer): integer; override;
    function GetDefaultFont: TFont; override;
    function GetDefaultFontColor: TColor; override;
    function GetLineHeight(AElement: TREElement): integer; override;
    function GetClientWidth: integer; override;
    function GetClientHeight: integer; override;
    function GetBulletChar(const AType: TBulletType): string; override;
{$IFDEF TMSPACK}
    function GetPictureByName(Container: TGDIPPictureContainer; const AName: string): TGDIPPicture;
{$ENDIF}
    procedure DrawErrorLine(ACanvas: TCanvas; x,w,y: integer; AColor: TColor);
    procedure DrawElement(ACanvas: TCanvas; el: TREElement; x, y, MaxLineWidth, LineHeight, LineDescent, LineBaseLine: integer; AValue: string; CharIndex, ExtraChar: integer); override;
    procedure DrawSelection(ACanvas: TCanvas; r: TRect); override;
    procedure DrawCaret(ACanvas: TCanvas); override;
    procedure DrawDragCaret(ACanvas: TCanvas); override;
    procedure DrawBackground(ACanvas: TCanvas); override;
    procedure DrawMargin(ACanvas: TCanvas); override;
    function DrawGraphic(ACanvas: TCanvas; ARect: TRect; ID: string): boolean; override;
    procedure DrawLineBreak(ACanvas: TCanvas; x,y: integer; el: TREElement); override;
    procedure MouseDownN(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMoveN(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUpN({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDownN(var Key: Word; {%H-}Shift: TShiftState); override;
    procedure KeyUpN(var {%H-}Key: Word; {%H-}Shift: TShiftState); override;
    procedure KeyPressN(var Key: Char); override;
    {$IFDEF LCLLIB}
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    {$ENDIF}
    {$IFDEF FMXLIB}
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    {$ENDIF}
    function DoDropFile(Filename: string): boolean; virtual;
    function DoDropImage(APicture: TPicture): boolean; virtual;
    procedure DoSelectionChanged; override;
    procedure DoDblClick(X,Y: integer); override;
    procedure DoContextForWord(MousePos: TPoint; AWord: string; AElement: TREElement; var Handled: Boolean); virtual;
    procedure DoClickHyperlink(URL, AValue: string); virtual;
    procedure DoClickGraphic(AElement: TREElement); virtual;
    procedure DoCanSelectGraphic(AElement: TREElement; var Allow: boolean); virtual;
    procedure DoCanSizeGraphic(AElement: TREElement; var Allow: boolean); virtual;
    procedure DoPaintEditor; override;
    procedure DoPasteText(var AText: string; var Allow: boolean); virtual;
    procedure DoPasteBitmap(ABitmap: TBitmap; var Allow: boolean); virtual;
    procedure DoPasteFormattedText(AStream: TStream); virtual;
    procedure DoImageNotFound(ImageName: string; var replaceImage: string); virtual;
    procedure DoPopupMenu(Sender: TObject);
    procedure DoPopupOpen(Sender: TObject);
    function GetVersionString:string; virtual;

    procedure CaretTimerProc(Sender: TObject);
    function IsEmoticon(const {%H-}EmoticonID: string): boolean; override;
    procedure ParseHTML(HTMLValue: string;
      {$IFNDEF FMXLIB}const Images: TCustomImageList = nil;{$ENDIF} const Pictures: TGDIPPictureContainer = nil);
    procedure CopyToClipboardStr(s: string); override;
    procedure CopyPictureToClipboard(APicture: TGDIPPicture); override;
    procedure PasteFromClipboardStream(AStream: TMemoryStream);
    procedure InitializePictureSize({%H-}NamedPictureElement: TNamedPictureElement); override;
    function HintAtXY(X,Y: integer): string;

    procedure Refresh; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure InitContextMenu; virtual;
    {$IFDEF USEVCLSTYLES}
    procedure InitVCLStyle(init: boolean);
    {$ENDIF}
    {$IFDEF FMXLIB}
    procedure InitStyle; override;
    procedure ResetToDefaultStyle; override;
    {$ENDIF}
    {$IFDEF FNCLIB}
    {$IFNDEF FMXLIB}
    procedure SetAdaptToStyle(const Value: boolean); override;
    {$ENDIF}
    {$ENDIF}
    procedure BeginPaintBuffer; override;
    procedure EndPaintBuffer; override;
    function CreateBuffer: TBitmap; override;
    procedure UpdateSelection; override;
    {$IFNDEF FMXLIB}
    procedure CreateWnd; override;
    {$ENDIF}
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function SetRange(ARange: TSize): boolean; override;
    procedure SetRichEditorPopup(AValue: TAdvRichEditorPopup);
    procedure SetCaptureN;
    procedure ReleaseCaptureN;
    procedure UpdateN;
    function DrawTextN(ACanvas: TCanvas; s: string; r: TRect; {%H-}Flags: DWORD): integer; overload;
    function DrawTextN(ACanvas: TCanvas; {%H-}s: string; r: TRectF; {%H-}Flags: DWORD): integer; overload;
    procedure UpdateCursor(ACursor: TCursor);
    procedure RestoreCursor;
    function PaintToInternal(ACanvas: TCanvas; ARect: TRect; Calc: boolean): integer;
    {$IFDEF FNCLIB}
    // ITMSFNCCustomEditor interface
    procedure SetText(AValue: String);
    procedure SetSelStart(AValue: Integer);
    procedure SetSelLength(AValue: Integer);
    function GetTextLength: Integer;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Print;
    procedure PaintTo(ACanvas: TCanvas; ARect: TRect);
    function CalcHeight(ACanvas: TCanvas; ARect: TRect): integer;
    property ZoomFactor: double read FZoomFactor write SetZoomFactor;
    property SingleLine: boolean read FSingleLine write FSingleLine;
    procedure AddToolBar(AToolBar: ITMSRichEditorToolBar);
    procedure RemoveToolBar(AToolBar: ITMSRichEditorToolBar);
    procedure CopyFormattedSelectionToClipboard;
    procedure PasteFormattedSelectionFromClipboard;
    procedure CopyRTFToClipboard;
    procedure CopyHTMLToClipboard;

    procedure CutToClipboard; override;
    procedure CopyToClipboard; override;
    function PasteFromClipboard: string; override;
    function ClipboardHasContent: boolean; override;
    property ClipboardFormats: TClipboardFormats read FClipboardFormats write FClipboardFormats;
    property CaretTimer: TTimer read FCaretTimer;
    property TabSize;
    property BulletSpacing;
    property OnImageNotFound: TImageNotFoundEvent read FOnImageNotFound write FOnImageNotFound;
  published
    property Align;
    property AllowSelect: boolean read FAllowSelect write FAllowSelect default True;
    property Anchors;
    property AutoCorrect;
    property BorderStyle;
    property Color default clWhite;
  {$IFNDEF FMXLIB}
    property DragCursor;
    property DragKind;
    property DragMode;
  {$ENDIF}
    property Enabled;
    property GraphicSelection: TGraphicSelection read FGraphicSelection write SetGraphicSelection;
    property Font;
  {$IFNDEF FMXLIB}
    property ParentFont;
  {$ENDIF}
  {$IFDEF TMSPACK}
    property Emoticons: TGDIPPictureContainer read FEmoticons write FEmoticons;
    property PictureContainer: TGDIPPictureContainer read FGDIPPictureContainer write FGDIPPictureContainer;
  {$ENDIF}
  {$IFNDEF FMXLIB}
    property PopupMenu;
  {$ENDIF}
    property ReadOnly;
  {$IFNDEF FNCLIB}
    property PopupToolBar: TAdvRichEditorPopup read FRichEditorPopup write SetRichEditorPopup;
  {$ENDIF}
  {$IFDEF FNCLIB}
    property ToolBarPopup: TTMSFNCRichEditorCustomToolBarPopup read FToolBarPopup write FToolBarPopup;
  {$ENDIF}
    property ShowHint;
  {$IFDEF VCLLIB}
  {$IFDEF DELPHIXE6_LVL}
    property StyleElements;
  {$ENDIF}
  {$ENDIF}
    property TabOrder;
    property TabStop;
  {$IFNDEF FNCLIB}
    property Touch;
  {$ENDIF}
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property WantTab default false;

    property OnCanSelectGraphic: TGraphicAllowEvent read FOnCanSelectGraphic write FOnCanSelectGraphic;
    property OnCanSizeGraphic: TGraphicAllowEvent read FOnCanSizeGraphic write FOnCanSizeGraphic;
    property OnCaretChanged;
  {$IFNDEF FMXLIB}
    property OnClick;
  {$ENDIF}
    property OnClickGraphic: TGraphicClickEvent read FOnClickGraphic write FOnClickGraphic;
    property OnClickHyperlink: THyperlinkClickEvent read FOnClickHyperlink write FOnClickHyperlink;

  {$IFNDEF FMXLIB}
    property OnContextPopup;
  {$ENDIF}
    property OnContextForWord: TWordContextEvent read FOnContextForWord write FOnContextForWord;
    property OnCorrectWord;
    property OnDblClick;
    property OnDrawGraphic: TRichEditorDrawGraphicEvent read FOnDrawGraphic write FOnDrawGraphic;
    property OnDrawBackground: TRichEditorDrawBackgroundEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDropFile: TDropFileEvent read FOnDropFile write FOnDropFile;
    property OnDropImage: TDropImageEvent read FOnDropImage write FOnDropImage;
  {$IFNDEF FMXLIB}
    property OnDragOver;
    property OnDragDrop;
  {$ENDIF}
    property OnEnter;
    property OnEnterWord;
    property OnExit;
  {$IFNDEF FMXLIB}
    property OnEndDock;
    property OnEndDrag;
  {$ENDIF}
    property OnGetElementHint: TGetElementHintEvent read FOnGetElementHint write FOnGetElementHint;
  {$IFNDEF LCLLIB}
    property OnGesture;
    property OnMergeField;
  {$IFNDEF FMXLIB}
    property OnMouseActivate;
  {$ENDIF}
  {$ENDIF}
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseWheel;
  {$IFNDEF FMXLIB}
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  {$ENDIF}
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
  {$IFNDEF FMXLIB}
    property OnKeyPress;
  {$ENDIF}
    property OnPasteBitmap: TPasteBitmapEvent read FOnPasteBitmap write FOnPasteBitmap;
    property OnPasteText: TPasteTextEvent read FOnPasteText write FOnPasteText;
    property OnPasteFormattedText: TPasteFormattedTextEvent read FOnPasteFormattedText write FOnPasteFormattedText;
    property OnSelectionChanged;
  {$IFNDEF FMXLIB}
    property OnStartDock;
    property OnStartDrag;
  {$ENDIF}
  end;

resourcestring
  SRECopy   = 'Copy';
  SRECut    = 'Cut';
  SREPaste  = 'Paste';
  SREClear  = 'Clear';
  SREAlign  = 'Align';
  SRELeft   = 'Left';
  SRECenter = 'Center';
  SRERight  = 'Right';


implementation

uses
  SysUtils, Math
{$IFNDEF LCLLIB}
  , StrUtils
{$ENDIF}
{$IFDEF VCLLIB}
  , ShellAPI, CommCtrl, ActiveX
{$ENDIF}
{$IFDEF USEVCLSTYLES}
  , VCL.Themes
{$ENDIF}
{$IFDEF FMXLIB}
  , FMX.TMSFNCStyles, System.UIConsts
{$ENDIF}
  ;

const
{$IFDEF FREEWARE}
  trialversion = ' trial version ';
{$ENDIF}

{$IFDEF FMXLIB}
  FontStyleUnderline = TFontStyle.fsUnderline;
  FontStyleItalic = TFontStyle.fsItalic;
  FontStyleBold = TFontStyle.fsBold;
  FontStyleStrikeOut = TFontStyle.fsStrikeOut;
{$ENDIF}

{$IFNDEF FMXLIB}
  FontStyleUnderline = fsUnderline;
{$ENDIF}

{$IFDEF FNCLIB}
  TBRESNAMECUT = 'TMSFNCTOOLBARCUT';
  TBRESNAMECOPY ='TMSFNCTOOLBARCOPY';
  TBRESNAMEPASTE = 'TMSFNCTOOLBARPASTE';
{$ENDIF}

{$IFNDEF FNCLIB}
  TBRESNAMECUT = 'TMSRETBCUT';
  TBRESNAMECOPY ='TMSRETBCOPY';
  TBRESNAMEPASTE = 'TMSRETBPASTE';
{$ENDIF}

{$IFDEF LCLLIB}
const
  DT_EXTERNALLEADING = $200;
{$IFNDEF FNCLIB}
  CF_UNICODETEXT = 13;
  CF_HDROP = 15;
{$ENDIF}
{$ENDIF}

procedure TAdvRichEditor.BeginPaintBuffer;
{$IFDEF FMXLIB}
var
  sc: Single;
  abs: TMatrix;
  s: TPointF;
  clipr: TClipRects;
const
  BaseVector: TPointF = (X: 0; Y: 0);
{$ENDIF}
begin
{$IFNDEF FMXLIB}
  Buffer.Width := GetClientWidth;
  Buffer.Height := GetClientHeight;
{$ENDIF}

{$IFDEF FMXLIB}
  if Assigned(Buffer) then
  begin
    if Assigned(Scene) then
      sc := Scene.GetSceneScale
    else
      sc := 1;

    abs := AbsoluteMatrix;
    s.X := (PointF(1, 0) * abs).Distance(BaseVector * abs);
    s.Y := (PointF(0, 1) * abs).Distance(BaseVector * abs);
    Buffer.Width := Round(GetClientWidth * sc * s.X);
    Buffer.Height := Round(GetClientHeight * sc * s.Y);
    Buffer.BitmapScale := sc;
    SetLength(clipr, 1);
    clipr[0] := RectF(0, 0, GetClientWidth * s.X - 1, GetClientHeight * s.Y - 1);
    Buffer.Canvas.BeginScene(@clipr);
    Buffer.Canvas.SetMatrix(Buffer.Canvas.Matrix.CreateScaling(s.X, s.Y));
  end;
{$ENDIF}
end;

procedure TAdvRichEditor.CaretTimerProc(Sender: TObject);
begin
  if not FDoCaret and not Focused then
    Exit;

  FDoCaret := not FDoCaret;
  FCaretUpdate := true;
  DoPaintEditor;
  FCaretUpdate := false;
end;


function TAdvRichEditor.HintAtXY(X,Y: integer): string;
var
  el: TREElement;
  SP: TPoint;
  s: string;
begin
  Result := Hint;

  el := nil;

  SP := TopLeft;

  XYToElement(X + SP.X, Y + SP.Y,el);

  if Assigned(el) then
  begin
    s := el.Hint;
    if Assigned(OnGetElementHint) then
      OnGetElementHint(Self, el, s);

    if s <> '' then
      Result := s;
  end;
end;

function TAdvRichEditor.ClipboardHasContent: boolean;
{$IFNDEF FNCLIB}
var
  Format: word;
{$ENDIF}
begin
  Result := false;

  {$IFNDEF FNCLIB}
  OpenClipboard(Handle);

  Format := EnumClipboardFormats(0);
  while Format <> 0 do
  begin
    if ClipboardHasText(Format) then
      Result := True;
    if ClipboardHasBitmap(Format) then
      Result := True;
    if ClipboardHasPicture(Format) then
      Result := True;
    if ClipboardHasRTE(Format) then
      Result := True;

    if Result then
      break;
    Format := EnumClipboardFormats(Format);
  end;

  CloseClipboard;
  {$ENDIF}

  {$IFDEF FNCLIB}
  if TTMSFNCClipBoard.HasFormat(TTMSFNCClipBoardFormat.cfText) then
    Result := true;

  if TTMSFNCClipBoard.HasFormat(TTMSFNCClipBoardFormat.cfRTF) then
    Result := true;

  if TTMSFNCClipBoard.HasFormat(TTMSFNCClipBoardFormat.cfBitmap) then
    Result := true;

//  if TTMSFNCClipBoard.HasFormat(CF_Picture) then
//    Result := true;

  if TTMSFNCClipBoard.HasFormat(TTMSFNCClipBoardFormat.cfRichTextStream) then
    Result := true;
  {$ENDIF}
end;

{$IFNDEF FMXLIB}
procedure TAdvRichEditor.CMHintShow(var Msg: TMessage);
var
  PHI: PHintInfo;
begin
  PHI := TCMHintShow(Msg).HintInfo;

  PHI^.HintStr := HintAtXY(PHI^.CursorPos.X, PHI^.CursorPos.Y);

  FLastHint := PHI^.HintStr;
end;
{$ENDIF}

{$IFNDEF FMXLIB}
procedure TAdvRichEditor.CMColorChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) and not (csReading in ComponentState) then
    DefaultChanged := true;
end;
{$ENDIF}

{$IFDEF USEVCLSTYLES}
procedure TAdvRichEditor.CMStyleChanged(var Message: TMessage);
begin
  InitVCLStyle(true);
end;
{$ENDIF}

{$IFNDEF FMXLIB}
procedure TAdvRichEditor.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  {$IFNDEF FNCLIB}
  if not Visible and Assigned(PopupToolBar) and PopupToolBar.Visible then
    PopupToolBar.Hide;
  {$ENDIF}
end;

procedure TAdvRichEditor.CMWantSpecialKey(var Msg:TCMWantSpecialKey);
begin
  inherited;
  if (Msg.CharCode = VK_RETURN) then
    Msg.Result := 1;
end;
{$ENDIF}

procedure TAdvRichEditor.PasteFormattedSelectionFromClipboard;
{$IFNDEF FNCLIB}
var
  DataPtr: Pointer;
  Data: THandle;
  AStream: TMemoryStream;
{$ENDIF}
begin
{$IFNDEF FNCLIB}
  if Clipboard.HasFormat(ClipboardRTEFMT) then
  begin
    // this is the preferred format ??
    Clipboard.Open;

    try
      Data := GetClipboardData(ClipboardRTEFMT);
      if Data = 0 then
      begin
        Clipboard.Close;
        Exit;
      end;

      DataPtr := GlobalLock(Data);
      if DataPtr = nil then
      begin
        Clipboard.Close;
        Exit;
      end;

      AStream := TMemoryStream.Create;
      try
        AStream.WriteBuffer(DataPtr^, GlobalSize(Data));
        AStream.Position := 0;

        PasteFromClipboardStream(AStream);
      finally
        AStream.Free;
      end;
    finally
      Clipboard.Close;
    end;
  end;
{$ENDIF}
end;

procedure TAdvRichEditor.CopyFormattedSelectionToClipboard;
{$IFNDEF FNCLIB}
var
  ms: TMemoryStream;
  Data: THandle;
  DataPtr: Pointer;
{$ENDIF}
begin

{$IFNDEF FNCLIB}
  Clipboard.Open;
  ms := TMemoryStream.Create;

  try
    SaveSelectionToStream(ms);
    ms.Position := 0;

    Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, ms.Size);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(ms.Memory^, DataPtr^, ms.Size);
        SetClipboardData(ClipboardRTEFMT, Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    ms.Free;
    Clipboard.Close;
  end;
{$ENDIF}
end;


procedure TAdvRichEditor.CopyHTMLToClipboard;
var
{$IFNDEF FNCLIB}
  MemHandleHTML: THandle;
  htmlstr: ansistring;
{$ENDIF}
{$IFDEF FNCLIB}
  clp: TTMSFNCClipBoard;
  htmlstr: string;
{$ENDIF}
begin
{$IFNDEF FNCLIB}
  htmlstr := AnsiString(GetContentAsHTML(False));
  htmlstr := AnsiString('Version:0.9'#13#10
{$ENDIF}
{$IFDEF FNCLIB}
  htmlstr := GetContentAsHTML(False);
  htmlstr := String('Version:0.9'#13#10
{$ENDIF}

  // standard HTML clipboard format preablme
    +'StartHTML:71'#13#10
    +'EndHTML:'+inttostr(length(htmlstr) + 71)+#13#10
    +'StartFragment:140'#13#10
    +'EndFragment:'+inttostr(length(htmlstr)+ 142)+#13#10
    +'<!DOCTYPE>' +#13#10
    +'<HTML>'#13#10
    +'<HEAD>'#13#10
    +'<TITLE>The HTML Clipboard</TITLE>'#13#10
    +'<BASE HREF="http://sample/specs">'#13#10
    +'</HEAD>'#13#10
    +'<BODY>'#13#10
    +'<!--StartFragment -->'#13#10)

{$IFNDEF FNCLIB}
    + htmlstr + AnsiString(#13#10
{$ENDIF}
{$IFDEF FNCLIB}
    + htmlstr + String(#13#10
{$ENDIF}
    +'<!--StartFragment -->'#13#10
    +'</BODY>'#13#10
    +'</HTML>'#13#10);

  {$IFNDEF FNCLIB}
  MemHandleHTML := GlobalAlloc(GHND or GMEM_SHARE, Length(htmlstr) + 1);

  if (MemHandleHTML <> 0) then
  begin
    {$IFDEF DELPHIXE4_LVL}
    AnsiStrings.StrCopy(GlobalLock(MemHandleHTML), PAnsiChar(htmlstr));
    {$ENDIF}
    {$IFNDEF DELPHIXE4_LVL}
    StrCopy(GlobalLock(MemHandleHTML), PAnsiChar(htmlstr));
    {$ENDIF}
    GlobalUnlock(MemHandleHTML);
    Clipboard.SetAsHandle(ClipboardHTMLFMT, MemHandleHTML);
  end;
  {$ENDIF}

  {$IFDEF FNCLIB}
  clp := TTMSFNCClipboard.Create;
  clp.SetHTML(htmlstr);
  clp.Free;
  {$ENDIF}
end;

procedure TAdvRichEditor.CutToClipboard;
begin
  inherited;
end;

procedure TAdvRichEditor.CopyToClipboard;
begin
  CopyToClipboardStr(SelectedText);
end;

procedure TAdvRichEditor.CopyToClipboardStr(s: string);
{$IFDEF FNCLIB}
var
  clp: TTMSFNCClipboard;
  ms: TMemoryStream;
{$ENDIF}
begin
{$IFDEF FNCLIB}
  clp := TTMSFNCClipBoard.Create;
  clp.SetText(s);
  clp.SetRTF(GetContentAsRTF(true));

  ms := TMemoryStream.Create;
  try
    SaveSelectionToStream(ms);
    ms.Position := 0;
    clp.SetRichTextStream(ms);
  finally
    ms.Free;
  end;

  clp.Free;
{$ENDIF}

{$IFNDEF FNCLIB}
  if Context.Content.Count > 0 then
  begin
    Clipboard.Open;

    Clipboard.AsText := s;

    PushContext;

    CopyFormattedSelectionToClipboard;

    CopyRTFToClipboard;

    PopContext;

    Clipboard.Close;
  end;
{$ENDIF}
end;

procedure TAdvRichEditor.CopyPictureToClipboard(APicture: TGDIPPicture);
{$IFNDEF FNCLIB}
var
  pic: TGraphic;
  ms: TMemoryStream;
{$ENDIF}
{$IFDEF FNCLIB}
var
  clp: TTMSFNCClipBoard;
{$ENDIF}

begin
  {$IFNDEF FNCLIB}
  pic := nil;

  case APicture.PictureFormat of
  pfBMP: pic := Graphics.TBitmap.Create;
  pfJPG: pic := TJPEGImage.Create;
  pfPNG: pic := TPngImage.Create;
  pfGIF: pic := TGIFImage.Create;
  pfICO: pic := TIcon.Create;
  end;

  if Assigned(pic) then
  begin
    try
      ms := TMemoryStream.Create;
      try
        APicture.SaveToStream(ms);
        ms.Position := 0;
        pic.LoadFromStream(ms);
      finally
        ms.Free;
      end;

      Clipboard.Open;
      Clipboard.Assign(pic);
      Clipboard.Close;

    finally
      pic.Free;
    end;
  end;
  {$ENDIF}

  {$IFDEF FNCLIB}
  if Assigned(APicture) then
  begin
    clp := TTMSFNCClipBoard.Create;
    clp.SetBitmap(APicture);
    clp.Free;
  end;
  {$ENDIF}
end;

procedure TAdvRichEditor.PasteFromClipboardStream(AStream: TMemoryStream);
var
  mss: TStateSaver;
  ms: TMemoState;
  f: single;
begin
  if HasSelection then
    DeleteSelection;

  mss := TStateSaver.Create(nil);
  ms := TMemoState.Create;
  mss.SaveState := ms;
  try
    AStream.ReadComponent(mss);
    LoadMemoState(ms, True);
  finally
    mss.Free;
    ms.Free;
  end;

  f := 1;

  {$IFNDEF FNCLIB}
  if (Producer = pFMX) then
    f := 72/96;
  {$ENDIF}

  DoPasteFormattedText(AStream);
  InsertFromStream(AStream, f);
end;

procedure TAdvRichEditor.CopyRTFToClipboard;
var
{$IFNDEF FNCLIB}
  rtfstr: ansistring;
  MemHandleRTF: THandle;
{$ENDIF}
{$IFDEF FNCLIB}
  rtfstr: string;
  clp: TTMSFNCClipboard;
{$ENDIF}
begin
  {$IFNDEF FNCLIB}
  rtfstr := ansistring(GetContentAsRTF(true));
  {$ENDIF}

  {$IFDEF FNCLIB}
  rtfstr := GetContentAsRTF(true);
  {$ENDIF}

  {$IFNDEF FNCLIB}
  MemHandleRTF := GlobalAlloc(GHND or GMEM_SHARE, Length(rtfstr) + 1);

  if (MemHandleRTF <> 0) then
  begin
    {$IFDEF DELPHIXE4_LVL}
    AnsiStrings.StrCopy(GlobalLock(MemHandleRTF), PAnsiChar(rtfstr));
    {$ENDIF}
    {$IFNDEF DELPHIXE4_LVL}
    StrCopy(GlobalLock(MemHandleRTF), PAnsiChar(rtfstr));
    {$ENDIF}
    GlobalUnlock(MemHandleRTF);
    Clipboard.SetAsHandle(ClipboardRTFFMT, MemHandleRTF);
  end;
  {$ENDIF}

  {$IFDEF FNCLIB}
  clp := TTMSFNCClipBoard.Create;
  clp.SetRTF(rtfstr);
  clp.Free;
  {$ENDIF}
end;

constructor TAdvRichEditor.Create(AOwner: TComponent);
begin
  inherited;
  Cursor := crIBeam;
  FOrigCursor := crNone;
  FOrigIBeam := True;
  FToolBarList := TToolBarList.Create;
  FDropTargetAssigned := false;
  FLastHint := '';
  {$IFNDEF FMXLIB}
  DoubleBuffered := true;
  {$ENDIF}
  Color := clWhite;
  SetFontColor(clBlack);
  Width := 400;
  Height := 300;
  FZoomFactor := 1.0;
  TabStop := true;
  FInternalDD := false;
  FCacheFont := TFont.Create;
  FCacheFont.Size := 1;
  FCaretTimer := TTimer.Create(Self);
  FCaretTimer.Interval := 500;
  FCaretTimer.OnTimer := CaretTimerProc;
  if (csDesigning in ComponentState) then
    FCaretTimer.Enabled := false;

  if (csDesigning in ComponentState) then
  begin
    FPopupMenu := TPopupMenu.Create(Self);
    {$IFNDEF FMXLIB}
    FImageList := TImageList.Create(Self);
    {$ENDIF}
  end
  else
  begin
    FPopupMenu := TPopupMenu.Create(AOwner);
    {$IFNDEF FMXLIB}
    FImageList := TImageList.Create(AOwner);
    {$ENDIF}
  end;

  FCutAction := TAdvRichEditorCut.Create(Self);
  FCutAction.Caption := SRECut;
  FCutAction.ImageIndex := 0;

  FCopyAction := TAdvRichEditorCopy.Create(Self);
  FCopyAction.Caption := SRECopy;
  FCopyAction.ImageIndex := 1;

  FPasteAction := TAdvRichEditorPaste.Create(Self);
  FPasteAction.Caption := SREPaste;
  FPasteAction.ImageIndex := 2;

  FLeftAction := TAdvRichEditorAlignLeft.Create(Self);
  FLeftAction.Caption := SRELeft;

  FCenterAction := TAdvRichEditorAlignCenter.Create(Self);
  FCenterAction.Caption := SRECenter;

  FRightAction := TAdvRichEditorAlignRight.Create(Self);
  FRightAction.Caption := SRERight;

  FClearAction := TAdvRichEditorClear.Create(Self);
  FClearAction.Caption := SREClear;
  FClearAction.ImageIndex := -1;

  FGraphicSelection := TGraphicSelection.Create;
  FSingleLine := False;
  FAllowSelect := True;

{$IFDEF TMSPACK}
  FGDIPPicture := TGDIPPicture.Create;
{$ENDIF}

  FClipboardFormats := [cfRTE, cfRTF, cfText, cfHTML, cfFile, cfBMP];
  InitContextMenu;
  CreateTextService;
end;

function TAdvRichEditor.CreateBuffer: TBitmap;
begin
  Result := TBitmap.Create;
end;


{$IFNDEF FMXLIB}
procedure TAdvRichEditor.CreateWnd;
begin
  inherited;
  {$IFNDEF FNCLIB}
  FDropTarget := TAdvRichEditorDropTarget.Create(Self);
  FDropTargetAssigned := RegisterDragDrop(Handle, FDropTarget) = S_OK;
  {$ENDIF}

  {$IFDEF USEVCLSTYLES}
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    InitVCLStyle(false);
  {$ENDIF}
end;
{$ENDIF}

destructor TAdvRichEditor.Destroy;
begin
  FCacheFont.Free;
  FGraphicSelection.Free;
  FCutAction.Free;
  FCopyAction.Free;
  FPasteAction.Free;
  FClearAction.Free;
  FreeAndNil(FToolBarList);
{$IFNDEF LCLLIB}
  FPopupMenu.Free;
{$ENDIF}
{$IFNDEF FMXLIB}
{$IFNDEF LCLLIB}
  FImageList.Free;
{$ENDIF}
{$ENDIF}
  FCaretTimer.Free;
{$IFDEF TMSPACK}
  FGDIPPicture.Free;
{$ENDIF}
  inherited;
end;

procedure TAdvRichEditor.Loaded;
begin
  inherited;
  DefaultChanged := false;
  {$IFDEF USEVCLSTYLES}
  InitVCLStyle(false);
  {$ENDIF}

  FOrigIBeam := Cursor = crIBeam;
end;

procedure TAdvRichEditor.DoClickGraphic(AElement: TREElement);
begin
  if Assigned(OnClickGraphic) then
    OnClickGraphic(Self, AElement);
end;

procedure TAdvRichEditor.DoCanSelectGraphic(AElement: TREElement; var Allow: boolean);
begin
  if Assigned(OnCanSelectGraphic) then
    OnCanSelectGraphic(Self, AElement, Allow);
end;

procedure TAdvRichEditor.DoCanSizeGraphic(AElement: TREElement; var Allow: boolean);
begin
  if Assigned(OnCanSizeGraphic) then
    OnCanSizeGraphic(Self, AElement, Allow);
end;

procedure TAdvRichEditor.DoClickHyperlink(URL, AValue: string);
begin
  if (URL = '#') then
    URL := AValue;

  if Assigned(OnClickHyperlink) then
    OnClickHyperlink(Self, URL)
  else
  begin
    {$IFDEF VCLLIB}
    ShellExecute(0,'open',PChar(URL),nil,nil,SW_NORMAL);
    {$ENDIF}
  end;
end;

procedure TAdvRichEditor.DoEnter;
begin
  inherited;
  if not Assigned(Caret.Element) or (Caret.CharIndex = -1) then
  begin
    Caret.Element := nil;
    Caret.CharIndex := 0;
  end;

  Refresh;

  UpdateTextService;
end;

procedure TAdvRichEditor.DoExit;
begin
  inherited;
  if not (csDestroying in ComponentState) then
    Refresh;
end;

function StrPScan(s: string; ch: char; FromPos: integer): integer;
var
  i: integer;
begin
  Result := -1;

  for i := FromPos to Length(s) do
  begin
    if CharInStr(s, i) = ch then
    begin
      Result := i;
      break;
    end;
  end;
end;

function StripQuotes(s: string): string;
var
  ch: char;
begin
  if Length(s) >= 2 then
  begin
    ch := CharInStr(s,1);

    if (ch = '"') or (ch = '''') then
    begin
      Delete(s,1,1);
    end;

    ch := CharInStr(s, Length(s));

    if (ch = '"') or (ch = '''') then
    begin
      Delete(s,Length(s),1);
    end;
  end;

  Result := s;
end;

function StripNonNumeric(s: string): string;
var
  ch: char;
  i: integer;
begin
  Result := '';

  for i := 1 to Length(s) do
  begin
    ch := CharInStr(s,1);

    if (ch >= '0') and (ch <= '9') then
      Result := Result + ch;
  end;
end;

function Hex2Color(s: string): TColor;
var
  r,g,b: Integer;
begin
  r := Hexval(Copy(s,1,2));
  g := Hexval(Copy(s,3,2)) shl 8;
  b := Hexval(Copy(s,5,2)) shl 16;
  Result := TColor(b + g + r);
end;

function HTMLColor(s: string): TColor;
begin
  // official HTML color names lookup

  if pos('CL',s) = 1 then
    Delete(s,1,2);

  Result := clNone;
  case IndexText(s, ['INDIANRED', 'LIGHTCORAL', 'SALMON', 'DARKSALMON', 'LIGHTSALMON', 'CRIMSON', 'RED', 'FIREBRICK',
                     'DARKRED', 'PINK', 'LIGHTPINK', 'HOTPINK', 'DEEPPINK', 'MEDIUMVIOLETRED', 'PALEVIOLETRED', 'LIGHTSALMON',
                     'CORAL', 'TOMATO', 'ORANGERED', 'DARKORANGE', 'ORANGE', 'GOLD', 'YELLOW', 'LIGHTYELLOW', 'LEMONCHIFFON',
                     'LIGHTGOLDENRODYELLOW', 'PAPAYAWHIP', 'MOCCASIN', 'PEACHPUFF', 'PALEGOLDENROD', 'KHAKI', 'DARKKHAKI', 'LAVENDER',
                     'THISTLE', 'PLUM', 'VIOLET', 'ORCHID', 'FUCHSIA', 'MAGENTA', 'MEDIUMORCHID', 'MEDIUMPURPLE', 'AMETHYST',
                     'BLUEVIOLET', 'DARKVIOLET', 'DARKORCHID', 'DARKMAGENTA', 'PURPLE', 'INDIGO', 'SLATEBLUE', 'DARKSLATEBLUE',
                     'MEDIUMSLATEBLUE', 'GREENYELLOW', 'CHARTREUSE', 'LAWNGREEN', 'LIME', 'LIMEGREEN', 'PALEGREEN', 'LIGHTGREEN',
                     'MEDIUMSPRINGGREEN', 'SPRINGGREEN', 'MEDIUMSEAGREEN', 'SEAGREEN', 'FORESTGREEN', 'GREEN', 'DARKGREEN',
                     'YELLOWGREEN', 'OLIVEDRAB', 'OLIVE', 'DARKOLIVEGREEN', 'MEDIUMAQUAMARINE', 'DARKSEAGREEN', 'LIGHTSEAGREEN',
                     'DARKCYAN', 'TEAL', 'AQUA', 'CYAN', 'LIGHTCYAN', 'PALETURQUOISE', 'AQUA', 'ARINE', 'TURQUOISE', 'MEDIUMTURQUOISE',
                     'DARKTURQUOISE', 'CADETBLUE', 'STEELBLUE', 'LIGHTSTEELBLUE', 'POWDERBLUE', 'LIGHTBLUE', 'SKYBLUE', 'LIGHTSKYBLUE',
                     'DEEPSKYBLUE', 'DODGERBLUE', 'CORNFLOWERBLUE', 'MEDIUMSLATEBLUE', 'ROYALBLUE', 'BLUE', 'MEDIUMBLUE', 'DARKBLUE',
                     'NAVY', 'MIDNIGHTBLUE', 'CORNSILK', 'BLANCHEDALMOND', 'BISQUE', 'NAVAJOWHITE', 'WHEAT', 'BURLYWOOD', 'TAN',
                     'ROSYBROWN', 'SANDYBROWN', 'GOLDENROD', 'DARKGOLDENROD', 'PERU', 'CHOCOLATE', 'SADDLEBROWN', 'SIENNA', 'BROWN',
                     'MAROON', 'WHITE', 'SNOW', 'HONEYDEW', 'MINTCREAM', 'AZURE', 'ALICEBLUE', 'GHOSTWHITE', 'WHITESMOKE', 'SEASHELL',
                     'BEIGE', 'OLDLACE', 'FLORALWHITE', 'IVORY', 'ANTIQUEWHITE', 'LINEN', 'LAVENDERBLUSH', 'MISTYROSE', 'GAINSBORO',
                     'LIGHTGREY', 'SILVER', 'DARKGRAY', 'GRAY', 'DIMGRAY', 'LIGHTSLATEGRAY', 'SLATEGRAY', 'DARKSLATEGRAY', 'BLACK']) of
    0: Result := $5C5CCD;
    1: Result := $8080F0;
    2: Result := $7280FA;
    3: Result := $7A96E9;
    4: Result := $7AA0FF;
    5: Result := $3C14DC;
    6: Result := $0000FF;
    7: Result := $2222B2;
    8: Result := $00008B;
    9: Result := $CBC0FF;
    10: Result := $C1B6FF;
    11: Result := $B469FF;
    12: Result := $9314FF;
    13: Result := $8515C7;
    14: Result := $9370DB;
    15: Result := $7AA0FF;
    16: Result := $507FFF;
    17: Result := $4763FF;
    18: Result := $0045FF;
    19: Result := $008CFF;
    20: Result := $00A5FF;
    21: Result := $00D7FF;
    22: Result := $00FFFF;
    23: Result := $E0FFFF;
    24: Result := $CDFAFF;
    25: Result := $D2FAFA;
    26: Result := $D5EFFF;
    27: Result := $B5E4FF;
    28: Result := $B9DAFF;
    29: Result := $AAE8EE;
    30: Result := $8CE6F0;
    31: Result := $6BB7BD;
    32: Result := $FAE6E6;
    33: Result := $D8BFD8;
    34: Result := $DDA0DD;
    35: Result := $EE82EE;
    36: Result := $D670DA;
    37: Result := $FF00FF;
    38: Result := $FF00FF;
    39: Result := $D355BA;
    40: Result := $DB7093;
    41: Result := $CC6699;
    42: Result := $E22B8A;
    43: Result := $D30094;
    44: Result := $CC3299;
    45: Result := $8B008B;
    46: Result := $800080;
    47: Result := $82004B;
    48: Result := $CD5A6A;
    49: Result := $8B3D48;
    50: Result := $EE687B;
    51: Result := $2FFFAD;
    52: Result := $00FF7F;
    53: Result := $00FC7C;
    54: Result := $00FF00;
    55: Result := $32CD32;
    56: Result := $98FB98;
    57: Result := $90EE90;
    58: Result := $9AFA00;
    59: Result := $7FFF00;
    60: Result := $71B33C;
    61: Result := $578B2E;
    62: Result := $228B22;
    63: Result := $008000;
    64: Result := $006400;
    65: Result := $32CD9A;
    66: Result := $238E6B;
    67: Result := $008080;
    68: Result := $2F6B55;
    69: Result := $AACD66;
    70: Result := $8FBC8F;
    71: Result := $AAB220;
    72: Result := $8B8B00;
    73: Result := $808000;
    74: Result := $FFFF00;
    75: Result := $FFFF00;
    76: Result := $FFFFE0;
    77: Result := $EEEEAF;
    78: Result := $D4FF7F;
    79: Result := $D0E040;
    80: Result := $CCD148;
    81: Result := $D1CE00;
    82: Result := $A09E5F;
    83: Result := $B48246;
    84: Result := $DEC4B0;
    85: Result := $E6E0B0;
    86: Result := $E6D8AD;
    87: Result := $EBCE87;
    88: Result := $FACE87;
    89: Result := $FFBF00;
    90: Result := $FF901E;
    91: Result := $ED9564;
    92: Result := $EE687B;
    93: Result := $E16941;
    94: Result := $FF0000;
    95: Result := $CD0000;
    96: Result := $8B0000;
    97: Result := $800000;
    98: Result := $701919;
    99: Result := $DCF8FF;
    100: Result := $CDEBFF;
    101: Result := $C4E4FF;
    102: Result := $ADDEFF;
    103: Result := $B3DEF5;
    104: Result := $87B8DE;
    105: Result := $8CB4D2;
    106: Result := $8F8FBC;
    107: Result := $60A4F4;
    108: Result := $20A5DA;
    109: Result := $0B86B8;
    110: Result := $3F85CD;
    111: Result := $1E69D2;
    112: Result := $13458B;
    113: Result := $2D52A0;
    114: Result := $2A2AA5;
    115: Result := $000080;
    116: Result := $FFFFFF;
    117: Result := $FAFAFF;
    118: Result := $F0FFF0;
    119: Result := $FAFFF5;
    120: Result := $FFFFF0;
    121: Result := $FFF8F0;
    122: Result := $FFF8F8;
    123: Result := $F5F5F5;
    124: Result := $EEF5FF;
    125: Result := $DCF5F5;
    126: Result := $E6F5FD;
    127: Result := $F0FAFF;
    128: Result := $F0FFFF;
    129: Result := $D7EBFA;
    130: Result := $E6F0FA;
    131: Result := $F5F0FF;
    132: Result := $E1E4FF;
    133: Result := $DCDCDC;
    134: Result := $D3D3D3;
    135: Result := $C0C0C0;
    136: Result := $A9A9A9;
    137: Result := $808080;
    138: Result := $696969;
    139: Result := $998877;
    140: Result := $908070;
    141: Result := $4F4F2F;
    142: Result := $000000;
  end;
end;

function HTML2Color(s: string): TColor;
begin
  if Pos('#',s) = 1 then
  begin
    Delete(s,1,1);
    Result := Hex2Color(s);
  end
  else
    Result := HTMLColor(s);
end;

procedure TAdvRichEditor.ParseHTML(HTMLValue: string;
  {$IFNDEF FMXLIB}const Images: TCustomImageList;{$ENDIF} const Pictures: TGDIPPictureContainer);
var
  TagName,TagP, sIdx,clr,fface,fsize,aclass,imgw,imgh,TagPV,TagPN: string;
  TagParams: TStringList;
  fSkipInitWhitespace, fPar, fSpace: Boolean;
  fBullet: TBulletType;
  fParAlign: TAlignment;
  mParAlign: TAlignment;
  {$IFDEF TMSPACK}
  APicture: TGraphic;
  {$ENDIF}
  LastColor,LastBkColor: TColor;
  LastFace: string;
  LastSize: integer;
  BulletIndex: integer;
  bs: TBulletStart;
  be: TBulletEnd;
  bu: TBulletElement;
  PP,PT,PP2,FSI,IDXTXT: integer;
  alignchange: boolean;
  hypchange: boolean;
  appleclass: boolean;
  isDoc, isXML, isFace: boolean;
  hypsel,el,lel: TREElement;
  hypselidx,specialidx: integer;
  hypurl,specialstr,ltag,replaceimage: string;
  ppchar,stylechar: char;
  imgstream: TStream;
  tagc: char;
  htmlnumchar: boolean;
  sq,dq,hn,he: integer;

begin
  // supports these tags:
  // B, I, U, S, A, P, BR, IMG, STRONG, EM, SUP, SUB, UL, OL, IL
  // &amp; &lt; &gt; &quot;   "

  if (HtmlValue = '') then
    Exit;

  //Clear;

  FBullet := GetSelectionBullet;
  FSkipInitWhitespace := True;       // skip initial white spaces
  FSpace := False;            // insert a single space
  FPar := False;              // insert a new paragraph
  alignchange := false;
  mParAlign := taLeftJustify;
  hypurl := '';
  hypsel := nil;
  hypselidx := 0;
  hypchange := false;
  appleclass := false;
  isDoc := false;
  isXML := false;

  //if IsSelectionCenter then
  //  FParAlign := taCenter
  //else
  //  if IsSelectionRight then
  //    FParAlign := taRightJustify
  //  else
      FParAlign := taLeftJustify;


  LastColor := GetDefaultFontColor;
  LastBkColor := Color;

  SelectionType := stSingle;

  TagParams := nil;
  BulletIndex := 0;

  HTMLValue := HTMLValue + #0;

  BeginUpdate;

  try
    TagParams := TStringList.Create;

    PP := 1;

    while (PP <= Length(HtmlValue)) do
    begin
      if (CharInStr(HtmlValue, PP) = '<') then
      begin
        // handle a tag
        Inc(PP);

        PT := PP;

        // scan till end of tag name
        while not IsCharInStr(CharInStr(HtmlValue,PP), '> '#0'['#10#13) do
          Inc(PP);

        TagName := Copy(HtmlValue, PT, PP - PT);
        TagParams.Clear;

        // scan till end of tag
        while (not IsCharInStr(CharInStr(HtmlValue, PP), '>'#0)) do
        begin
          // eat spaces & comments
          while IsCharInStr(CharInStr(HtmlValue, PP), ' /') do
            Inc(PP);

          PT := PP;

          // scan attributes with quotes
          while not IsCharInStr(CharInStr(HtmlValue,PP), '>'#0'"''') do
          begin
            Inc(PP);
            ppchar := CharInStr(HtmlValue,PP);
            if IsCharInStr(ppchar, '"''') then
            begin
              PP2 := StrPScan(HtmlValue, ppchar, PP + 1);

              if PP2 <> -1 then
                PP := PP2;

              Inc(PP);
            end;
          end;

          if (PP <> PT) then
          begin
            TagP := Copy(HtmlValue, PT, PP - PT);

            sq := pos('''', TagP);
            dq := pos('"', TagP);

            TagC := '"';
            if (dq = 0) and (sq > 0) then
              TagC := '''';

            if (dq > 0) and (sq > 0)  then
            begin
              if sq < dq then
                TagC := '''';
            end;


            while pos(TagC, TagP) > 0 do
            begin
              TagPN := Trim(Copy(TagP, 1, pos(TagC, TagP)));
              Delete(TagP, 1, pos(TagC, TagP));

              if pos(TagC, TagP) > 0 then
              begin
                TagPV := Copy(TagP, 1, pos(TagC, TagP));
                Delete(TagP, 1, pos(TagC, TagP));
                TagParams.Add(TagPN + TagPV);
              end;
            end;

            TagParams.Add(TagP);
          end
          else
            inc(PP);
        end;

        if (CharInStr(HtmlValue,PP) = '>') then
        begin
          Inc(PP);
          if isDoc then
            isDoc := false;
        end
        else
          break; //unclosed left parenthesis

        IDXTXT := IndexText(TagName, ['B', '/B', 'STRONG', '/STRONG', 'I', '/I',
                                  'EM', '/EM', 'U', '/U', 'P', '/P', 'BR',
                                  'IMG', 'S', '/S', 'SUP', '/SUP', 'SUB',
                                  '/SUB', 'UL', 'OL', '/UL', '/OL', 'LI', '/LI',
                                  'FONT','/FONT','SPAN','/SPAN','A','/A',
                                  'XML','/XML','!--','STYLE','/STYLE','BODY','/BODY','TITLE','/TITLE','STRIKE','/STRIKE']);

        if (IDXTXT in [0..10]) then
        begin
          if FSpace then
            InsertChar(#32);
          FSpace := false;
        end;

        case IDXTXT of
           0, 2: SetSelectionBold(True);
           1, 3: SetSelectionBold(False);
           4, 6: SetSelectionItalic(True);
           5, 7: SetSelectionItalic(False);
           8: SetSelectionUnderline(True);
           9: SetSelectionUnderline(False);
           10:
             begin                 { <P> }
               fSpace := False;
               fBullet := btNone;
               mParAlign := taLeftJustify;
               case IndexText(StripQuotes(TagParams.Values['ALIGN']), ['center', '"center"','right', '"right"']) of
               0, 1: mParAlign := taCenter;
               2, 3: mParAlign := taRightJustify;
               end;
               if not fSkipInitWhitespace or (fParAlign <> mParAlign) or fPar then
               begin
                 alignchange := true;
                 lel := LastElement;
                 if Assigned(lel) and not (lel is TLineBreakElement) then AppendLineBreak;
                 //SetSelectionAttribute(mParAlign);
                 fParAlign := mParAlign;
                 //fSkipInitWhitespace := True;
                 fPar := False;
                 fSpace := False;
               end;

               clr := StripQuotes(TagParams.Values['BGCOLOR']);
               if (clr <> '') then
               begin
                 SetSelectionBkColor(HTML2Color(Uppercase(clr)));
               end;

             end;
           11:
             begin
               // linebreak just before paragraph close is superfluous
               el := LastElement;
               if Assigned(el) and (el is TLineBreakElement) then
               begin
                 Context.Content.Remove(el);
                 el.Free;
               end;

               fPar := True;       { </P> }
               fSpace := False;
               if mParAlign <> taLeftJustify then
               begin
                 mParAlign := taLeftJustify;
                 alignchange := true;
               end;
               fSkipInitWhitespace := True;
             end;
           12:
             begin
               AppendLineBreak; { <BR> }
               SetSelectionAttribute(fParAlign);
               fPar := False;
               fSpace := False;
               fSkipInitWhitespace := True;
             end;
           13:
             begin                  { <IMG> }
               sIdx := StripQuotes(TagParams.Values['SRC']);

               imgw := StripQuotes(TagParams.Values['WIDTH']);
               imgh := StripQuotes(TagParams.Values['HEIGHT']);

               if pos('file://', sIdx) = 1 then
               begin
                 Delete(sIdx,1,7);

                 if not FileExists(sIdx) then
                 begin
                   replaceimage := '';
                   DoImageNotFound(sIdx, replaceimage);
                   if (replaceimage <> '') and FileExists(replaceimage) then
                     sIdx := replaceimage;
                 end;

                 if FileExists(sIdx) then
                 begin
                   if (imgw <> '') and (imgh <> '') then
                   begin
                     imgw := StripNonNumeric(imgw);
                     imgh := StripNonNumeric(imgh);

                     InsertImage(sIdx, StrToInt(imgw), StrToInt(imgh));
                   end
                   else
                     InsertImage(sIdx);

                   SetCaret(cpEndDoc);
                 end;
               end
               else
               if pos('data:', sIdx) = 1 then
               begin
                 sIdx := Copy(sIdx, Pos('base64,',sIdx) + 7, Length(sIdx));

                 imgstream := DecodePicture(sIdx);
                 try
                   InsertImage(imgstream);
                 finally
                   imgstream.Free;
                 end;

               end
               else
               begin
                 if not FileExists(sIdx) then
                 begin
                   replaceimage := '';
                   DoImageNotFound(sIdx, replaceimage);
                   if (replaceimage <> '') and FileExists(replaceimage) then
                     sIdx := replaceimage;
                 end;

                 if FileExists(sIdx) then
                 begin
                   InsertImage(sIdx);
                   SetCaret(cpEndDoc);
                 end;
               end;

               if (Length(sIdx) >= 2) and (CharInStr(sIdx, 1) = CharInStr(sIdx,Length(sIdx))) and IsCharInStr(CharInStr(sIdx,1), '''"') then
                 sIdx := AnsiDequotedStr(sIdx, CharInStr(sIdx,1));
               if (fPar) then
               begin
                 AppendLineBreak;
                 SetSelectionAttribute(fParAlign);
                 fPar := False;
               end
               else if (FSpace) then
                 InsertText(' ');
               fSpace := False;
               {$IFNDEF FMXLIB}
               if Assigned(Images) and (Pos('IDX:',Uppercase(sIdx)) = 1) then
               begin
                 InsertGraphic(sIdx, Images.Width + 4, Images.Height + 4);
                 SetSelectionFontSize(Font.Size);
               end
               else if Assigned(Pictures) then
               begin
                 {$IFDEF TMSPACK}
                 APicture := Pictures.FindPicture(sIdx);
                 if Assigned(APicture) then
                 begin
                   InsertGraphic(sIdx, APicture.Width + 4, APicture.Height + 4);
                   SetSelectionFontSize(Font.Size);
                 end;
                 {$ENDIF}
               end;
               {$ENDIF}
             end;
           14: SetSelectionStrikeOut(True);
           15: SetSelectionStrikeOut(False);
           16: SetSelectionSuperscript(True);
           17: SetSelectionSuperscript(False);
           18: SetSelectionSubscript(True);
           19: SetSelectionSubscript(False);
           20:
             begin
               if not (LastElement is TLineBreakElement) then
                 AppendLineBreak;
               fBullet := btCircle; // UL

               bs := TBulletStart.Create;
               bs.Assign(LastElement);
               bs.Indent := 24;
               BulletIndex := 0;
               Context.Content.Add(bs);
             end;
           21:
             begin
               if not (LastElement is TLineBreakElement) then
                 AppendLineBreak;
               fBullet := btNumber; // OL
               BulletIndex := 0;
             end;
           22, 23:
             begin // /UL, /OL
               fBullet := btNone;
               fPar := True;
               fSkipInitWhitespace := True;

               be := TBulletEnd.Create;
               be.Assign(LastElement);
               be.Indent := 0;

               Context.Content.Add(be);
               //AppendLineBreak;
             end;
           24: // LI
             begin
               SetCaret(cpEndDoc);
               //TAdvRichEditorEx(RichEditor).InsertLineBreakAndBullet(fBullet, 24);

               if BulletIndex > 0 then
                 AppendLineBreak;

               bu := TBulletElement.Create;
               bu.Assign(LastElement);
               bu.&Type := fBullet;
               bu.Indent := 24;
               bu.Index := BulletIndex;
               bu.BulletFont := GetFontName(LastElement.Font);

               inc(BulletIndex);

               //SetCaret(cpEndDoc);
               //Caret.CharIndex := 0;

               Context.Content.Add(bu);
               Caret.Element := bu;
               Caret.CharIndex := 1;

               SetSelectionAttribute(fParAlign);
               fSpace := False;
               fPar := False;
               fSkipInitWhitespace := True;
             end;
           25:
             begin // /LI
               fPar := True;
               fSpace := False;
               fSkipInitWhitespace := True;
             end;
           26: //  FONT
             begin
               LastColor := GetSelectionTextColor;
               LastFace := GetSelectionFontName;
               LastSize := GetSelectionFontSize;
               clr := StripQuotes(TagParams.Values['COLOR']);
               if (clr <> '') then
               begin
                 SetSelectionColor(HTML2Color(Uppercase(clr)));
               end;
               clr := StripQuotes(TagParams.Values['BGCOLOR']);
               if (clr <> '') then
               begin
                 SetSelectionBkColor(HTML2Color(Uppercase(clr)));
               end;
               fface := StripQuotes(TagParams.Values['FACE']);
               if (fface <> '') then
               begin
                 SetSelectionFontName(fface);
               end;
               fsize := StripQuotes(TagParams.Values['SIZE']);
               if (fsize <> '') then
               begin
                 fsi := 1;
                 while IsNumChar(CharInStr(fsize,fsi)) do
                   inc(fsi);

                 fsize := Copy(fsize,1,fsi - 1);
                 fsi := Abs(StrToInt(fsize));
                 case fsi of
                 1: fsi := 6;
                 2: fsi := 8;
                 3: fsi := 10;
                 4: fsi := 14;
                 5: fsi := 18;
                 6: fsi := 24;
                 end;

                 SetSelectionFontSize(fsi);
               end;

               fsize := Uppercase(StripQuotes(TagParams.Values['STYLE']));
               if (fsize <> '') then
               begin
                 fsi := Pos('FONT-FAMILY:',fsize);
                 if (fsi > 0) then
                 begin
                   fface := Trim(Copy(fsize,fsi + 12, Length(fsize)));

                   fsi := 1;
                   isFace := true;

                   repeat
                     stylechar := CharInStr(fface,fsi);

                     if (fsi <= Length(fsize)) and (stylechar <> '"') and (stylechar <> ';') then
                       inc(fsi)
                     else
                       isFace := false;

                   until not isFace;

                   fface := Copy(fface,1,fsi - 1);
                   fface := StringReplace(fface, '''','',[rfReplaceAll]);
                   SetSelectionFontName(fface);
                 end;

                 fsi := Pos('FONT-SIZE:',fsize);

                 if (fsi > 0) then
                 begin
                   fsize := Trim(Copy(fsize,fsi + 10, Length(fsize)));

                   fsi := 1;
                   while (fsi <= Length(fsize)) and IsNumChar(CharInStr(fsize,fsi)) do
                     inc(fsi);

                   fsize := Copy(fsize,1,fsi - 1);

                   SetSelectionFontSize(strtoint(fsize));
                 end;
              end;

             end;
           27: //  /FONT
             begin
               SetSelectionColor(LastColor);
               SetSelectionFontName(LastFace);
               SetSelectionFontSize(LastSize);
             end;
           28:
             begin
               LastBkColor := GetSelectionBkColor;
               clr := Uppercase(TagParams.Values['STYLE']);
               aclass := TagParams.Values['CLASS'];
               if (aclass <> '') then
               begin
                 appleclass := pos('APPLE-', Uppercase(aclass)) > 0;
               end;

               if (clr <> '') then
               begin
                 //style="background-color:#FFFF00">

                 if pos('"BACKGROUND-COLOR:#',clr) = 1 then
                 begin
                   Delete(clr,1,Length('"BACKGROUND-COLOR:'));
                   Delete(clr,Length(clr),1);
                   SetSelectionBkColor(HTML2Color(clr));
                 end;

                 if pos('COLOR:',clr) = 2 then
                 begin
                   Delete(clr,1,1 + Length('COLOR:'));
                   Delete(clr,Length(clr),1);
                   SetSelectionColor(HTML2Color(clr));
                 end;
               end;
             end;
           29:
             begin
               appleclass := false;
               SetSelectionBkColor(LastBkColor);
             end;
           30:   // start hyperlink
             begin
               hypchange := true;
               hypurl := StripQuotes(TagParams.Values['HREF']);
             end;
           31:   // end hyperlink
             begin
               SetCaret(cpEndDoc);

               Selection.FromElement := hypsel;
               Selection.FromChar := hypselidx;
               Selection.ToElement := Caret.Element;
               Selection.ToChar := Caret.CharIndex;

               SetSelectionHyperlink(hypurl);
               SetCaret(cpEndDoc);
               DefaultChanged := true;
             end;
           32: // <XML>
             begin
               isXML := true;
             end;
           33: // </XML>
             begin
               isXML := false;
             end;
           34: // <!--
             begin
               isDoc := true;
             end;
           35:
             begin
               isDoc := true;
             end;
           36:
             begin
               isDoc := false;
             end;
           37:
             begin
               clr := StripQuotes(TagParams.Values['BGCOLOR']);
               if (clr <> '') then
               begin
                 SetSelectionBkColor(HTML2Color(Uppercase(clr)));
               end;

               clr := Uppercase(StripQuotes(TagParams.Values['STYLE']));
               begin
                 fsize := clr;
                 fsi := Pos('FONT-SIZE:',fsize);

                 if (fsi > 0) then
                 begin
                   fsize := Trim(Copy(fsize,fsi + 10, Length(fsize)));

                   fsi := 1;
                   while IsNumChar(CharInStr(fsize,fsi)) do
                     inc(fsi);

                   fsize := Copy(fsize,1,fsi - 1);

                   SetFontSize(Font, StrToInt(fsize));
                 end;

                 fsize := clr;
                 fsi := Pos('FONT-FAMILY:',fsize);

                 if (fsi > 0) then
                 begin
                   fsize := Trim(Copy(fsize,fsi + 12, Length(fsize)));

                   if CharInStr(fsize,1) = '"' then
                     Delete(fsize,1,1);

                   if CharInStr(fsize,1) = '''' then
                     Delete(fsize,1,1);

                   fsi := 1;
                   while IsAlphaChar(CharInStr(fsize,fsi)) do
                     inc(fsi);

                   fsize := Copy(fsize,1,fsi - 1);

                   SetFontName(Font, fsize);
                 end;

                 fsize := clr;
                 fsi := Pos('FONT-STYLE:',fsize);

                 if (fsi > 0) then
                 begin
                   fsize := Trim(Copy(fsize,fsi + 11, Length(fsize)));

                   if CharInStr(fsize,1) = '"' then
                     Delete(fsize,1,1);

                   if CharInStr(fsize,1) = '''' then
                     Delete(fsize,1,1);

                   fsi := 1;
                   while IsAlphaChar(CharInStr(fsize,fsi)) do
                     inc(fsi);

                   fsize := Copy(fsize,1,fsi - 1);

                   Font.Style := [];

                   if Pos('OBLIQUE', fsize) > 0 then
                   {$IFDEF DELPHIXE3_LVL}
                     Font.Style := Font.Style + [TFontStyle.fsBold];
                   {$ENDIF}
                   {$IFNDEF DELPHIXE3_LVL}
                     Font.Style := Font.Style + [fsBold];
                   {$ENDIF}
                   if Pos('ITALIC', fsize) > 0 then
                   {$IFDEF DELPHIXE3_LVL}
                     Font.Style := Font.Style + [TFontStyle.fsItalic];
                   {$ENDIF}
                   {$IFNDEF DELPHIXE3_LVL}
                     Font.Style := Font.Style + [fsItalic];
                   {$ENDIF}
                 end;
               end;
             end;
           38:
             begin

             end;
           39:
             begin
               isDoc := true;
             end;
           40:
             begin
               isDoc := false;
             end;
           41:
             begin
               SetSelectionStrikeOut(true);
             end;
           42:
             begin
               SetSelectionStrikeOut(false);
             end;

           // ignore other tags
        end;
      end
      else if (Ord(CharInStr(HtmlValue,PP)) <= 32) then
      begin
        fSpace := not fSkipInitWhitespace;
        Inc(PP);
      end
      else
        if appleclass then
        begin
          while (CharInStr(HtmlValue, PP) <> '<') do Inc(PP);
            InsertChar(' ');
        end
        else
        if (CharInStr(HtmlValue, PP) = '&') then
        begin
          Inc(PP);
          PT := PP;

          htmlnumchar := false;

          if CharInStr(HtmlValue, PP) = '#' then
          begin
            inc(pp);
            while IsNumChar(CharInStr(HtmlValue, PP)) do Inc(PP);
            if (CharInStr(HtmlValue, PP) = ';') then
            begin
              Inc(PP);
              TagName := Copy(HtmlValue, PT + 1, PP - PT - 2);
              Val(TagName, hn, he);
              if he = 0 then
              begin
                specialstr := chr(hn);
                htmlnumchar := true;

                if (fSpace) then
                  InsertChar(' ');
                fSpace := false;

                PT := PP;
              end;
            end
            else
              PP := PT;
          end;

          if not htmlnumchar then
          begin

            while IsAlphaChar(CharInStr(HtmlValue, PP)) do Inc(PP);
            if (CharInStr(HtmlValue, PP) = ';') then
              Inc(PP)
            else
              PP := PT;

            if (fPar) then
            begin
              AppendLineBreak;
              SetSelectionAttribute(fParAlign);
              fPar := False;
            end
            else
              if (fSpace) then
                InsertChar(' ');

            fSpace := False;
            fSkipInitWhitespace := False;

            TagName := Copy(HtmlValue, PT, PP - PT);

            ltag := LowerCase(TagName);

            if not (ltag = 'quot;') and not (ltag = 'lt;') and not (ltag = 'gt;') and not (ltag = 'amp;') and not (ltag = '') then
              TagName := '&' + TagName;

            specialidx := TAdvUtils.GetMarkupIndex(TagName);

            if (specialidx >= 0) then
            {$IFDEF LCLLIB}
              specialstr := UTF8Encode(TAdvUtils.GetSpecialChar(specialidx + 1))
            {$ENDIF}
            {$IFNDEF LCLLIB}
              specialstr := TAdvUtils.GetSpecialChar(specialidx + 1)
            {$ENDIF}
            else
              specialstr := '&';
          end;

          if not (isDoc or isXML) then
          {$IFDEF LCLLIB}
            InsertChar(specialstr);
          {$ENDIF}
          {$IFNDEF LCLLIB}
            InsertChar(CharInstr(specialstr,1));
          {$ENDIF}

          if alignchange then
          begin
            SetSelectionAttribute(mParAlign);
            alignchange := false;
          end;

          SetCaret(cpEndDoc);

          if hypchange then
          begin
            hypsel := Caret.Element;
            hypselidx := Caret.CharIndex - 1;
            hypchange := false;
          end;

        end
        else
        begin
          if not (isDoc or isXML) then
          begin

            if (fPar) then
            begin
              AppendLineBreak;
              SetSelectionAttribute(fParAlign);
              fPar := False;
            end
            else if (fSpace) then
              InsertChar(' ');

            fSpace := False;
            fSkipInitWhitespace := False;
            InsertChar(CharInStr(HtmlValue, PP));

            if alignchange then
            begin
              SetSelectionAttribute(mParAlign);
              alignchange := false;
            end;

            SetCaret(cpEndDoc);

            if hypchange then
            begin
              hypsel := Caret.Element;
              hypselidx := Caret.CharIndex - 1;
              hypchange := false;
            end;
          end;

          Inc(PP);
        end;
    end;

    if fPar then
    begin
      AppendLineBreak;
    end
    else
    if fSpace then
    begin
      //InsertChar(' ');
      //SetCaret(cpEndDoc);
    end;

  finally
    TagParams.Free;
    SelectionType := TSelectionType.stDefault;
    EndUpdate;
  end;

end;

function TAdvRichEditor.IsEmoticon(const EmoticonID: string): boolean;
{$IFDEF TMSPACK}
var
  i: integer;
{$ENDIF}
begin
  Result := false;
{$IFDEF TMSPACK}
  if not Assigned(Emoticons) then
    Exit;

  if Emoticons.Items.Count = 0 then
    Exit;

  if EmoticonID = '' then
    Exit;

  for i := 0 to Emoticons.Items.Count - 1 do
  begin
    if (EmoticonID = Emoticons.Items[i].Name) then
    begin
      Result := true;
      break;
    end;
  end;
{$ENDIF}
end;

procedure TAdvRichEditor.DoSelectionChanged;
var
  I: Integer;
begin
  inherited;
  DefaultChanged := false;
  if Assigned(FToolBarList) then
    for I := 0 to FToolBarList.Count - 1 do
      FToolBarList[I].UpdateToolBar;
end;

function TAdvRichEditor.DoDropFile(Filename: string): boolean;
begin
  Result := true;

  if Assigned(OnDropFile) then
    OnDropFile(Self, Filename, Result);
end;

function TAdvRichEditor.DoDropImage(APicture: TPicture): boolean;
begin
  Result := true;

  if Assigned(OnDropImage) then
    OnDropImage(Self, APicture, Result);
end;


procedure TAdvRichEditor.DoDblClick(X,Y: integer);
begin
  if FAllowSelect then
  begin
    SelectWordAtXY(X, Y);
    FClickOnSel := false;
    FMouseDown := false;
    UpdateN;
    DoSelectionChanged;
  end;
end;


procedure TAdvRichEditor.DoContextForWord(MousePos: TPoint; AWord: string; AElement: TREElement; var Handled: Boolean);
begin
  if Assigned(OnContextCorrectWord) then
    OnContextCorrectWord(Self, MousePos, AWord, AElement, Handled);

  if Handled then
    Exit;

  if Assigned(OnContextForWord) then
    OnContextForWord(Self, MousePos, AWord, AElement, Handled);
end;

{$IFDEF FMXLIB}
procedure TAdvRichEditor.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  FDoCaret := false;
  FCaretTimer.Enabled := false;

  inherited;

  if not SingleLine then
  begin
    if WheelDelta < 0 then
    begin
      if Caret.LH > 0 then
        ScrollDown(Caret.LH)
      else
        ScrollDown(20);
    end
    else
    begin
       if Caret.LH > 0 then
        ScrollUp(Caret.LH)
      else
        ScrollUp(20);
    end;

    Refresh;
  end;

  FCaretTimer.Enabled := true;

  Handled := True;
end;
{$ENDIF}


{$IFNDEF FMXLIB}
procedure TAdvRichEditor.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
var
  s: string;
  el: TREElement;

begin
  el := nil;
  XYToElement(MousePos.X, MousePos.Y, el);

  if Assigned(el) then
  begin
    s := WordAtXY(MousePos.X, MousePos.Y);
    DoContextForWord(MousePos, s, el, Handled);
  end;

  if Handled then
    Exit;

  if not Assigned(PopupMenu) then
    PopupMenu := FPopupMenu;
  inherited;
end;


function TAdvRichEditor.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  FDoCaret := false;
  FCaretTimer.Enabled := false;

  Result := inherited;

  if FSingleLine then
    Exit;

  if Caret.LH > 0 then
    ScrollDown(Caret.LH)
  else
    ScrollDown(20);

  Refresh;
  FCaretTimer.Enabled := true;
end;

function TAdvRichEditor.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  FDoCaret := false;
  FCaretTimer.Enabled := false;

  Result := inherited;

  if FSingleLine then
    Exit;

  if Caret.LH > 0 then
    ScrollUp(Caret.LH)
  else
    ScrollUp(20);

  Refresh;
  FCaretTimer.Enabled := true;
end;
{$ENDIF}


procedure TAdvRichEditor.DoPaintEditor;
begin
  {$IFNDEF FMXLIB}
  Invalidate;
  {$ENDIF}
  {$IFDEF FMXLIB}
  Repaint;
  {$ENDIF}
end;

procedure TAdvRichEditor.DoPopupOpen(Sender: TObject);
var
  I: Integer;
  mnu: TMenuItem;
begin
  if Assigned(FPopupMenu) then
  begin
    {$IFDEF FMXLIB}
    for I := 0 to FPopupMenu.ItemsCount - 1 do
    {$ENDIF}
    {$IFNDEF FMXLIB}
    for I := 0 to FPopupMenu.Items.Count - 1 do
    {$ENDIF}
    begin
      mnu := FPopupMenu.Items[I];
      case TPopupMenuAction(mnu.Tag) of
        pmCut, pmCopy: mnu.Enabled := HasSelection or Assigned(Selected);
        pmPaste: mnu.Enabled := ClipboardHasContent;
      end;
    end;
  end;
end;

procedure TAdvRichEditor.DoPopupMenu(Sender: TObject);
begin
  if not (Assigned(Sender) and (Sender is TMenuItem)) then
    Exit;

  case TPopupMenuAction((Sender as TMenuItem).Tag) of
  pmClear: Clear;
  pmCut: CutToClipboard;
  pmCopy: CopyToClipboard;
  pmPaste: PasteFromClipboard;
  pmAlignLeft: SetSelectionAttribute(TAlignment.taLeftJustify);
  pmAlignCenter: SetSelectionAttribute(TAlignment.taCenter);
  pmAlignRight: SetSelectionAttribute(TAlignment.taRightJustify);
  end;
end;

procedure TAdvRichEditor.DoImageNotFound(ImageName: string; var ReplaceImage: string);
begin
  if Assigned(OnImageNotFound) then
    OnImageNotFound(Self, ImageName, ReplaceImage);
end;

procedure TAdvRichEditor.DoPasteFormattedText(AStream: TStream);
begin
  if Assigned(OnPasteFormattedText) then
    OnPasteFormattedText(Self, AStream);
end;

procedure TAdvRichEditor.DoPasteText(var AText: string; var Allow: boolean);
begin
  if Assigned(OnPasteText) then
    OnPasteText(Self, AText, Allow);
end;

procedure TAdvRichEditor.DoPasteBitmap(ABitmap: TBitmap; var Allow: boolean);
begin
  if Assigned(OnPasteBitmap) then
    OnPasteBitmap(Self, ABitmap, Allow);
end;

procedure TAdvRichEditor.DrawBackground(ACanvas: TCanvas);
{$IFNDEF FMXLIB}
var
  r: TRect;
  sp: TPoint;
{$ENDIF}
{$IFDEF FMXLIB}
var
  r: TRectF;
  ri: TRect;
  sp: TPointF;
{$ENDIF}

begin
{$IFDEF FMXLIB}
  r := RectF(0, 0, GetClientWidth - 1, GetClientHeight - 1);
  sp := TopLeft;

  r.Top := r.Top - sp.Y;

  r.Left := r.Left + PageMargin.Horizontal;
  r.Right := r.Right - PageMargin.Horizontal;
  r.Top := r.Top + PageMargin.Vertical;

  ACanvas.Fill.Assign(Fill);
  ACanvas.Stroke.Assign(Stroke);
  ACanvas.FillRect(r, 0, 0, AllCorners, AbsoluteOpacity);
  ACanvas.DrawRect(r, 0, 0, AllCorners, AbsoluteOpacity);

  ri.Left := Round(r.Left);
  ri.Top := Round(r.Top);
  ri.Right := Round(r.Right);
  ri.Bottom := Round(r.Bottom);
  if Assigned(OnDrawBackground) then
    OnDrawBackground(Self, ACanvas, ri);
{$ENDIF}

{$IFNDEF FMXLIB}
  r := ClientRect;
  sp := TopLeft;

  r.Top := r.Top - SP.Y;

  r.Left := r.Left + PageMargin.Horizontal;
  r.Right := r.Right - PageMargin.Horizontal;
  r.Top := r.Top + PageMargin.Vertical;

  ACanvas.Brush.Color := Color;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Color := ACanvas.Brush.Color;

  ACanvas.Rectangle(r);
  if Assigned(OnDrawBackground) then
    OnDrawBackground(Self, ACanvas, r);
{$ENDIF}
end;

{$IFNDEF FMXLIB}
procedure TAdvRichEditor.DrawCaret(ACanvas: TCanvas);
var
  ch,delta: integer;
  el: TREElement;
  tm: TTextMetric;
  SP: TPoint;

begin
  //if (Caret.XY.Y < 0) or (Caret.XY.Y > Height) then
  //  Exit;
  if ReadOnly then
    Exit;

  SP := TopLeft;

  el := NearbyTextElement(Caret.Element, 0);

  if Assigned(el) and (el is TTextElement) then
  begin
    SetFontName(ACanvas.Font, GetFontName((el as TTextElement).Font));
    ACanvas.Font.Size := GetFontSize((el as TTextElement).Font);
  end
  else
  begin
    if not Assigned(el) then
      el := PreviousElement(Caret.Element);

    if Assigned(el) then
    begin
      SetFontName(ACanvas.Font, GetFontName(el.Font));
      ACanvas.Font.Size := GetFontSize(el.Font);
    end
    else
    begin
      SetFontName(ACanvas.Font, GetFontName(Font));
      ACanvas.Font.Size := GetFontSize(Font);
    end;
  end;

  ch := ACanvas.TextHeight(TextForHeight);

  tm.tmDescent := 0;
  GetTextMetrics(ACanvas.Handle, TM);

  //TDebug.Write('caret descent:%d - %d %d  y:%d',[Caret.Descent, tm.tmDescent, tm.tmAscent, Caret.XY.y]);

  delta := Caret.Descent - tm.tmDescent;

  //TDebug.Write('draw caret: %d %d %d',[caret.XY.X, caret.XY.Y, Caret.LH]);

  if Caret.XY.Y + ch > 0 then
  begin
    ACanvas.Pen.Color := Caret.Color;
    ACanvas.Pen.Width := 2;
    ACanvas.Pen.Style := psSolid;
    ACanvas.MoveTo(Caret.XY.X - SP.X,Caret.XY.Y - SP.Y + Caret.LH - 1 - delta);
    ACanvas.LineTo(Caret.XY.X - SP.X,Caret.XY.Y - SP.Y + Caret.LH - ch - delta);
  end;
end;
{$ENDIF}

{$IFDEF FMXLIB}
procedure TAdvRichEditor.DrawCaret(ACanvas: TCanvas);
var
  ch,delta: integer;
  el: TREElement;
  SP: TPoint;
  tm: Integer;
begin
  if ReadOnly then
    Exit;

  SP := TopLeft;

  el := NearbyTextElement(Caret.Element, 0);

  if Assigned(el) and (el is TTextElement) then
  begin
    ACanvas.Font.Family := GetFontName((el as TTextElement).Font);
    ACanvas.Font.Size := GetFontSize((el as TTextElement).Font);
  end
  else
  begin
    if not Assigned(el) then
      el := PreviousElement(Caret.Element);

    if Assigned(el) then
    begin
      ACanvas.Font.Family := GetFontName(el.Font);
      ACanvas.Font.Size := GetFontSize(el.Font);
    end
    else
    begin
      ACanvas.Font.Family := GetFontName(Font);
      ACanvas.Font.Size := GetFontSize(Font);
    end;
  end;

  ch := Round(ACanvas.TextHeight(TextForHeight));
  tm := GetTextDescent(ACanvas, el);
  delta := Caret.Descent - tm;

  if Caret.XY.Y + ch > 0 then
  begin
    ACanvas.Stroke.Color := Caret.Color;
    ACanvas.Stroke.Kind := TBrushKind.Solid;
    {$IFDEF DELPHITOKYO}
    ACanvas.Stroke.Thickness := 2;
    ACanvas.Stroke.Dash := TStrokeDash.Solid;
    {$ELSE}
    ACanvas.StrokeThickness := 2;
    ACanvas.StrokeDash := TStrokeDash.Solid;
    {$ENDIF}
    ACanvas.DrawLine(PointF(Caret.XY.X - SP.X,Caret.XY.Y - SP.Y + Caret.LH - 1 - delta),
      PointF(Caret.XY.X - SP.X,Caret.XY.Y - SP.Y + Caret.LH - ch - delta), AbsoluteOpacity);
  end;
end;
{$ENDIF}

procedure TAdvRichEditor.DrawDragCaret(ACanvas: TCanvas);
var
  sp: TPoint;
begin
  SP := TopLeft;

  if (DragCaret.XY.Y - SP.Y < 0) or (DragCaret.XY.Y + DragCaret.LH - SP.Y > Height) then
    Exit;

{$IFNDEF FMXLIB}
  ACanvas.Pen.Color := Caret.Color;
  ACanvas.Pen.Width := 2;
  ACanvas.Pen.Style := psSolid;
  ACanvas.MoveTo(DragCaret.XY.X - SP.X,DragCaret.XY.Y - SP.Y + 1);
  ACanvas.LineTo(DragCaret.XY.X - SP.X,DragCaret.XY.Y - SP.Y + DragCaret.LH - 1);
{$ENDIF}
end;


{$IFDEF VCLLIB}
function TAdvRichEditor.GetCharPos(AValue: string; CharIndex: integer): integer;
var
  nnfit,mw: integer;
  charpos: array[0..2047] of integer;
  sz: TSize;
begin
  if Assigned(Caret.Element) then
  begin
    if (Caret.Element is TTextElement) then
      Canvas.Font.Assign((Caret.Element as TTextElement).Font)
    else
      Canvas.Font.Assign(Font);

    Canvas.Font.Size := Round(Canvas.Font.Size * FZoomFactor);
  end;

  mw := GetLineWidth;
  GetTextExtentExPoint(Canvas.Handle, PChar(AValue + ' '), Min(2047,Length(AValue) + 1), mw, @nnfit, @charpos, sz);

  if (charindex > 2047)  then
    charindex := 2047;

  if charindex < 1 then
    charindex := 1;

  Result := charpos[CharIndex - 1];
end;
{$ENDIF}

{$IFNDEF VCLLIB}
function TAdvRichEditor.GetCharPos(AValue: string; CharIndex: integer): integer;
var
  su: string;
  rctout, r: TRectF;
begin
  if Assigned(Caret.Element) then
  begin
    if (Caret.Element is TTextElement) then
      Canvas.Font.Assign((Caret.Element as TTextElement).Font)
    else
      Canvas.Font.Assign(Font);
  end;

  su := Copy(AValue, 1, CharIndex);

  rctout := RectF(0,0,GetLineWidth,GetClientHeight);
  r := CalculateTextR(Canvas, su, rctout, false);

  Result := Round(r.Right);
end;
{$ENDIF}

{$IFNDEF FMXLIB}
procedure TAdvRichEditor.DrawElement(ACanvas: TCanvas; el: TREElement; x, y, MaxLineWidth, LineHeight, LineDescent, LineBaseLine: integer; AValue: string; CharIndex, ExtraChar: integer);
var
  r: TRect;
  tm: TTextMetric;
  pic: TGDIPPicture;
  selword: boolean;
  preValue,postValue,Value: string;
  lv,tw,pw,ph: integer;
  TEXTFLAGS: DWORD;
  iserr: boolean;
  sz: TTextSize;
  fcs: TFontCharset;
  dpi: double;
  bmp: TBitmap;
  origw,origh: integer;

begin
  TEXTFLAGS := DT_EDITCONTROL or DT_SINGLELINE or DT_LEFT or DT_BOTTOM or DT_EXTERNALLEADING or DT_NOPREFIX;

  {
  if (el = Selection.ToElement) then
     TDebug.Write('FROM:'+Selection.FromElement.ClassName +':' +Selection.FromElement.Text);
  if (el = Selection.ToElement) then
     TDebug.Write('TO:'+Selection.ToElement.ClassName +':' +Selection.ToElement.Text);
  }

  if (el is TBulletElement) then
  begin
    r := Rect(x,y,x + $FFFF, y + LineHeight);

    if ((el as TBulletElement).&Type in [btCircle, btSquare, btStar, btTick, btArrow]) then
    begin
      sz := GetTextSize(ACanvas, el, 'W');
      r := Rect(x,y,x + sz.cx, y + LineHeight);
      TEXTFLAGS := DT_EDITCONTROL or DT_SINGLELINE or DT_CENTER or DT_BOTTOM or DT_EXTERNALLEADING or DT_NOPREFIX;
    end;


    fcs := ACanvas.Font.Charset;

    {$IFDEF LCLLIB}
    SetFontName(ACanvas.Font, 'Arial');
    {$ENDIF}

    {$IFNDEF LCLLIB}
    SetFontName(ACanvas.Font, 'Wingdings');
    {$ENDIF}

    ACanvas.Font.Charset := DEFAULT_CHARSET;

    Value := GetBulletChar((el as TBulletElement).&Type);
    {$IFDEF LCLLIB}
    Value := UTF8Encode(Value);
    {$ENDIF}

    case (el as TBulletElement).&Type of
    btNumber:
      begin
        Value := Format((el as TBulletElement).BulletFormat,[(el as TBulletElement).Index + 1]);
        SetFontName(ACanvas.Font, (el as TBulletElement).BulletFont);
      end;
    btChar:
      begin
        Value := Chr(ord('a')+(el as TBulletElement).Index);
        SetFontName(ACanvas.Font, (el as TBulletElement).BulletFont);
      end;
    btCustom:
      begin
        Value := (el as TBulletElement).Bullet;
        SetFontName(ACanvas.Font, (el as TBulletElement).BulletFont);
      end;
    end;

    ACanvas.Brush.Style := bsClear;

    if el.Color <> clNone then
    begin
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := el.Color;
    end;

    if (el.Baseline = tbRegular) and (LineBaseLine > 0)  then // there is subscript
    begin
      sz := GetTextSize(ACanvas, el, TextForHeight);
      r.Bottom := r.Bottom  - round(sz.cy / 4);
    end;

    if el.Baseline = tbSuperScript then
    begin
      r.Bottom := r.Bottom  - round(el.TH / 4);
      if LineBaseLine > 0 then
        r.Bottom := r.Bottom  - round(el.TH / 4);
    end;

    ACanvas.Font.Color := el.TextColor;
    ACanvas.Font.Style := el.Font.Style;
    ACanvas.Font.Size := GetFontSize(el.Font);
    ACanvas.Font.Style := ACanvas.Font.Style - [fsUnderline, fsStrikeOut];
    DrawTextN(ACanvas, Value, r, TEXTFLAGS);
    ACanvas.Font.Charset := fcs;
  end;

  if (el is TTextElement) then
  begin
    iserr := (el as TTextElement).Error;

    ACanvas.Font.Assign((el as TTextElement).Font);
    ACanvas.Font.Color := ColorToRGB((el as TTextElement).TextColor);

    ACanvas.Font.Size := Round(ACanvas.Font.Size * FZoomFactor);

    if (el.URL <> '') then
    begin
      ACanvas.Font.Color := URLColor;
      if URLUnderline then
        ACanvas.Font.Style := ACanvas.Font.Style + [FontStyleUnderline];
    end;

    tm.tmDescent := 0;
    GetTextMetrics(ACanvas.Handle, TM);

    y := y - (LineDescent - tm.tmDescent) + tm.tmExternalLeading;

    r := Rect(x,y,Min(x + $FFFF, MaxLineWidth), y + LineHeight);

    sz.cy := 0;

    if (el.Baseline = tbRegular) and (LineBaseLine > 0)  then // there is subscript
    begin
      r.Bottom := r.Bottom  - round(el.TH / 4);
    end;

    if el.Baseline = tbSuperScript then
    begin
      r.Bottom := r.Bottom  - round(el.TH / 4);
      if LineBaseLine > 0 then
        r.Bottom := r.Bottom  - round(el.TH / 4);
    end;

    lv := Length(AValue) - ExtraChar;

    selword := false;

    if el.Selected and not HideSelection then
    begin

      selword := true;

      if (el.SelFrom <> -1) and (CharIndex + lv <= el.SelFrom) then
        selword := false;

      if (el.SelTo <> -1) and (CharIndex >= el.SelTo) then
        selword := false;
    end;

    preValue := '';
    postValue := '';
    Value := AValue;

    if selword then
    begin
      if (CharIndex < el.SelTo) and (CharIndex + lv >= el.SelTo) then
      begin
        postValue := Copy(AValue, el.SelTo - CharIndex + 1, lv);
        Value := Copy(AValue, 1, el.SelTo - CharIndex);
      end;

      if (CharIndex < el.SelFrom) and (CharIndex + lv > el.SelFrom) then
      begin
        preValue := Copy(Value, 1, el.SelFrom - CharIndex);
        Value := Copy(Value,el.SelFrom - CharIndex + 1, lv);
      end;

      if ((el as TTextElement).Color <> clNone) then
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := (el as TTextElement).Color;
      end
      else
        ACanvas.Brush.Style := bsClear;

      if el.URL = '' then
        ACanvas.Font.Color := (el as TTextElement).TextColor;

      if preValue <> '' then
      begin
        if el.Highlight then
        begin
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Brush.Color := clYellow;
        end;

        if el.MergeRef <> '' then
        begin
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Brush.Color := clSilver;
          ACanvas.Font.Color := clBlack;
        end;

        if ((el as TTextElement).StyleTag = 1) then
          preValue := Uppercase(preValue)
        else
        if ((el as TTextElement).StyleTag = 2) then
          preValue := Lowercase(preValue);

        DrawTextN(ACanvas, preValue, r, TEXTFLAGS);
        tw := ACanvas.TextWidth(preValue);

        if iserr then
          DrawErrorLine(ACanvas, r.Left, tw, r.Bottom, clRed);

        if (el as TTextElement).Line then
        begin
          ACanvas.Pen.Color := clBlack;
          ACanvas.MoveTo(r.Left, r.Bottom);
          ACanvas.LineTo(MaxLineWidth, r.Bottom);
        end;

        r.Left := r.Left + tw;
      end;

      ACanvas.Brush.Color := SelectionColor;
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Font.Color := SelectionTextColor;

      if IsForwardSelection then
      begin
        if (el = Selection.FromElement) and (CharIndex <= Selection.FromChar) then
          Selection.FromXY := Point(r.Left, r.Top);
      end
      else
      begin
        if (el = Selection.ToElement) and (CharIndex <= Selection.ToChar) then
          Selection.FromXY := Point(r.Left, r.Top);
      end;

      if (Value <> '') then
      begin
        if ((el as TTextElement).StyleTag = 1) then
          Value := Uppercase(Value)
        else
        if ((el as TTextElement).StyleTag = 2) then
          Value := Lowercase(Value);

        DrawTextN(ACanvas, Value, r, TEXTFLAGS);

        tw := ACanvas.TextWidth(Value);

        if iserr then
          DrawErrorLine(ACanvas, r.Left, tw, r.Bottom, clRed);

        if (el as TTextElement).Line then
        begin
          ACanvas.Pen.Color := clBlack;
          ACanvas.MoveTo(r.Left, r.Bottom);
          ACanvas.LineTo(MaxLineWidth, r.Bottom);
        end;

        r.Left := r.Left + tw;
      end;

      if IsForwardSelection then
      begin
        if (el = Selection.ToElement) and (CharIndex < Selection.ToChar) then
          Selection.ToXY := Point(r.Left, r.Bottom);
      end
      else
      begin
        if (el = Selection.FromElement) and (CharIndex < Selection.FromChar) then
          Selection.ToXY := Point(r.Left, r.Bottom);
      end;

      if (el as TTextElement).Color <> clNone then
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := (el as TTextElement).Color;
      end
      else
        ACanvas.Brush.Style := bsClear;

      if el.URL = '' then
        ACanvas.Font.Color := (el as TTextElement).TextColor
      else
        ACanvas.Font.Color := URLColor;

      if postValue <> '' then
      begin
        if el.Highlight then
        begin
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Brush.Color := clYellow;
        end;

        if el.MergeRef <> '' then
        begin
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Brush.Color := clSilver;
          ACanvas.Font.Color := clBlack;
        end;

        if ((el as TTextElement).StyleTag = 1) then
          postValue := Uppercase(postValue)
        else
        if ((el as TTextElement).StyleTag = 2) then
          postValue := Lowercase(postValue);

        DrawTextN(ACanvas, postValue, r, TEXTFLAGS);
        tw := ACanvas.TextWidth(postValue);

        if iserr then
          DrawErrorLine(ACanvas, r.Left, tw, r.Bottom, clRed);

        if (el as TTextElement).Line then
        begin
          ACanvas.Pen.Color := clBlack;
          ACanvas.MoveTo(r.Left, r.Bottom);
          ACanvas.LineTo(MaxLineWidth, r.Bottom);
        end;

      end;
    end
    else
    begin
      if (el as TTextElement).Color <> clNone then
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := (el as TTextElement).Color;
      end
      else
        ACanvas.Brush.Style := bsClear;

      if el.Highlight then
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := clYellow;
      end;

      if el.MergeRef <> '' then
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := clSilver;
        ACanvas.Font.Color := clBlack;
      end;

      if ((el as TTextElement).StyleTag = 1) then
        AValue := Uppercase(AValue)
      else
      if ((el as TTextElement).StyleTag = 2) then
        AValue := Lowercase(AValue);

      // for printing
      r.Top := r.Bottom - round((r.Bottom - r.Top) * DPIRatio);

      DrawTextN(ACanvas, AValue, r, TEXTFLAGS);

      if iserr then
      begin
        tw := ACanvas.TextWidth(AValue);
        DrawErrorLine(ACanvas, r.Left, tw, r.Bottom, clRed);
      end;

      if (el as TTextElement).Line then
      begin
        ACanvas.Pen.Color := clBlack;
        ACanvas.MoveTo(r.Left, r.Bottom);
        ACanvas.LineTo(MaxLineWidth, r.Bottom);
      end;

    end;
  end
  else
  begin
    if el = Selection.FromElement then
    begin
      Selection.FromXY := Point(x, y);
    end;

    if el = Selection.ToElement then
    begin
      Selection.ToXY := Point(x,y + LineHeight);
    end;
  end;

  if (el is TLineElement) then
  begin
    ACanvas.Pen.Color := (el as TLineElement).Color;
    ACanvas.Pen.Width := (el as TLineElement).Width;
    ACanvas.Pen.Style := (el as TLineElement).Style;
    ACanvas.MoveTo(0, y + LineHeight div 2);
    ACanvas.LineTo(MaxLineWidth, y + LineHeight div 2);
  end;

  if (el is TCustomGraphicElement) then
  begin
    dpi := (el as TCustomGraphicElement).DPIratio;

    r := Rect(x,y,x + Round((el as TCustomGraphicElement).DrawSize.cx * dpi), y + Round((el as TCustomGraphicElement).DrawSize.cy * dpi));

    origw := r.Right - r.Left - GetPictureMargin;
    origh := r.Bottom - r.Top;

    InflateRect(r, -Round(GetPictureMargin * dpi) div 2, -Round(GetPictureMargin * dpi) div 2);

    if el.Selected and not HideSelection and HasTextSelection then
    begin
      ACanvas.Brush.Color := SelectionColor;
      ACanvas.Pen.Color := SelectionColor;
      ACanvas.Rectangle(r);
    end;

    if dpi <> 1.0 then
    begin
      ACanvas.Brush.Color := clWhite;
      ACanvas.Pen.Color := clWhite;
      ACanvas.Rectangle(r);
    end;

    pic := nil;
    pw := 0;
    ph := 0;

    {$IFDEF TMSPACK}
    if (el is TNamedPictureElement) then
    begin
      if (el is TEmoticonPictureElement) then
        pic := GetPictureByName(Emoticons, (el as TEmoticonPictureElement).Name)
      else
        pic := GetPictureByName(PictureContainer, (el as TNamedPictureElement).Name);

      if not Assigned(pic) then
      begin
        // No handler implemented for custom graphic
        ACanvas.Pen.Color := clRed;
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Style := psSolid;

        ACanvas.MoveTo(r.Left,r.Top);
        ACanvas.LineTo(r.Right,r.Bottom);

        ACanvas.MoveTo(r.Right, r.Top);
        ACanvas.LineTo(r.Left,r.Bottom);
      end
      else
      begin
        (el as TNamedPictureElement).Width := pic.Width;
        (el as TNamedPictureElement).Height := pic.Height;
      end;
    end;
    {$ENDIF}

    if (el is TPictureElement) then
    begin
      pic := (el as TPictureElement).Picture;
      ph := (el as TPictureElement).PictureHeight;
      pw := (el as TPictureElement).PictureWidth;
    end;

    if (el is TPictureElement) or (el is TNamedPictureElement) then
    begin
      {$IFNDEF FMXLIB}
      if Assigned(pic) and not pic.Empty then
      {$ENDIF}
      {$IFDEF FMXLIB}
      if Assigned(pic) and not pic.IsEmpty then
      {$ENDIF}
      begin
        if (dpi <> 1.0) then
        begin
          bmp := Graphics.TBitmap.Create;
          bmp.Transparent := false;
          bmp.Width := r.Right - r.Left;
          bmp.Height := r.Bottom - r.Top;
          bmp.Canvas.Brush.Color := clWhite;
          bmp.Canvas.Rectangle(r);
          bmp.Canvas.StretchDraw(Rect(0,0,bmp.Width,bmp.Height),pic.Graphic);
          ACanvas.Draw(r.Left,r.Top,bmp);
          bmp.Free;
        end
        else
        begin
          if (pw > 0) and (ph > 0) then
          begin
            if (origw = pic.Width) and (origh = pic.Height) then
              ACanvas.Draw(r.Left + Round(GetPictureMargin * dpi) div 2, r.Top + Round(GetPictureMargin * dpi) div 2, pic.Graphic)
            else
              ACanvas.StretchDraw(r, pic.Graphic)
          end
          else
          begin
            if dpi <> 1.0 then
              ACanvas.StretchDraw(r, pic.Graphic)
            else
              ACanvas.Draw(x + PICTURE_MARGIN div 2,y, pic.Graphic);
          end;
        end;
      end;
    end;

    if (el is TGraphicElement) then
    begin
      if not DrawGraphic(ACanvas, r, (el as TGraphicElement).ID) then
      begin
        // No handler implemented for custom graphic
        ACanvas.Pen.Color := clRed;
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Style := psSolid;

        ACanvas.MoveTo(r.Left,r.Top);
        ACanvas.LineTo(r.Right,r.Bottom);

        ACanvas.MoveTo(r.Right, r.Top);
        ACanvas.LineTo(r.Left,r.Bottom);
      end;
    end;

    if (el = Selected) then
    begin
      DrawSelection(ACanvas, r);
    end;

  end;
end;
{$ENDIF}

{$IFDEF FMXLIB}
procedure TAdvRichEditor.DrawElement(ACanvas: TCanvas; el: TREElement; x, y, MaxLineWidth, LineHeight, LineDescent, LineBaseLine: integer; AValue: string; CharIndex, ExtraChar: integer);
var
  r: TRectF;
  pic: TBitmap;
  selword: boolean;
  preValue,postValue,Value: string;
  lv: Integer;
  tw, pw, ph: Single;
  iserr: boolean;
  sz: TTextSize;
  c: TAlphaColor;
  dpi: Double;
  btType: TBulletType;
  bkgdrawn: Boolean;

  procedure DrawImeText(const AText: String; const AIsIME: Boolean);
  var
    beforeIME, afterIME: String;
    imeLeft, afterLeft: Single;
    imeR: TRectF;
  begin
    beforeIME := AText.SubString(0, FImeStartIndex);
    afterIME := AText.Substring(FImeStartIndex + Length(FImeString));

    imeLeft := CalculateText(ACanvas, beforeIME).Width;
    afterLeft := CalculateText(ACanvas, beforeIME + FImeString).Width;

    imeR := r;

    DrawTextN(ACanvas, beforeIME, imeR, 0);

    imeR.Left := r.Left + imeLeft;
    DrawTextN(ACanvas, FImeString, imeR, 0);

    imeR.Left := r.Left + afterLeft;
    DrawTextN(ACanvas, afterIME, imeR, 0);
  end;

begin
  if (el is TBulletElement) then
  begin
    r := RectF(x,y,x + $FFFF, y + LineHeight);

    UpdateBulletFont(ACanvas);

    btType := (el as TBulletElement).&Type;
    case btType of
    btCircle, btSquare, btStar, btArrow, btTick: Value := GetBulletChar(btType);
    btNumber:
      begin
        Value := Format((el as TBulletElement).BulletFormat,[(el as TBulletElement).Index + 1]);
        ACanvas.Font.Family := (el as TBulletElement).BulletFont;
      end;
    btChar:
      begin
        Value := Chr(ord('a')+(el as TBulletElement).Index);
        ACanvas.Font.Family := (el as TBulletElement).BulletFont;
      end;
    btCustom:
      begin
        Value := (el as TBulletElement).Bullet;
        ACanvas.Font.Family := (el as TBulletElement).BulletFont;
      end;
    end;

    if (el.Baseline = tbRegular) and (LineBaseLine > 0)  then // there is subscript
    begin
      sz := GetTextSize(ACanvas, el, TextForHeight);
      r.Bottom := r.Bottom  - round(sz.cy / 4);
    end;

    if el.Baseline = tbSuperScript then
    begin
      r.Bottom := r.Bottom  - round(el.TH / 4);
      if LineBaseLine > 0 then
        r.Bottom := r.Bottom  - round(el.TH / 4);
    end;

    tw := -1;

    if el.Color <> clNone then
    begin
      ACanvas.Fill.Color := el.Color;
      if tw = -1 then
        tw := CalculateText(ACanvas, Value).Width;
      ACanvas.FillRect(RectF(Floor(r.Left) + 0.5, r.Top, Round(r.Left + tw) + 1.5, r.Bottom), 0, 0, AllCorners, AbsoluteOpacity)
    end;

    ACanvas.Font.Style := el.Font.Style;
    ACanvas.Font.Size := GetFontSize(el.Font);
    ACanvas.Font.Style := ACanvas.Font.Style - [FontStyleUnderline, FontStyleStrikeOut];
    ACanvas.Fill.Color := el.TextColor;

    DrawTextN(ACanvas, Value, r, 0);
  end;

  if (el is TTextElement) then
  begin
    iserr := (el as TTextElement).Error;
    ACanvas.Font.Assign((el as TTextElement).Font);

    if (el.URL <> '') and URLUnderline then
      ACanvas.Font.Style := ACanvas.Font.Style + [FontStyleUnderline];

    y := y - (LineDescent - GetTextDescent(ACanvas, el));
    r := RectF(x,y,Min(x + $FFFF, MaxLineWidth), y + LineHeight);

    sz.cy := 0;

    if (el.Baseline = tbRegular) and (LineBaseLine > 0)  then // there is subscript
      r.Bottom := r.Bottom  - round(el.TH / 4);

    if el.Baseline = tbSuperScript then
    begin
      r.Bottom := r.Bottom  - round(el.TH / 4);
      if LineBaseLine > 0 then
        r.Bottom := r.Bottom  - round(el.TH / 4);
    end;

    lv := Length(AValue) - ExtraChar;
    selword := false;

    if el.Selected and not HideSelection then
    begin
      selword := true;

      if (el.SelFrom <> -1) and (CharIndex + lv <= el.SelFrom) then
        selword := false;

      if (el.SelTo <> -1) and (CharIndex >= el.SelTo) then
        selword := false;
    end;

    preValue := '';
    postValue := '';
    Value := AValue;

    if selword then
    begin
      if (CharIndex < el.SelTo) and (CharIndex + lv >= el.SelTo) then
      begin
        postValue := Copy(AValue, el.SelTo - CharIndex + 1, lv);
        Value := Copy(AValue, 1, el.SelTo - CharIndex);
      end;

      if (CharIndex < el.SelFrom) and (CharIndex + lv > el.SelFrom) then
      begin
        preValue := Copy(Value, 1, el.SelFrom - CharIndex);
        Value := Copy(Value,el.SelFrom - CharIndex + 1, lv);
      end;

      if preValue <> '' then
      begin
        tw := CalculateText(ACanvas, preValue).Width;

        bkgdrawn := False;

        if el.Highlight then
        begin
          bkgdrawn := True;
          c := ACanvas.Fill.Color;
          ACanvas.Fill.Color := clYellow;
          ACanvas.FillRect(RectF(Floor(r.Left) + 0.5, r.Top, Round(r.Left + tw) + 1.5, r.Bottom), 0, 0, AllCorners, AbsoluteOpacity);
          ACanvas.Fill.Color := c;
        end;

        if el.MergeRef <> '' then
        begin
          bkgdrawn := True;
          ACanvas.Fill.Color := clSilver;
          ACanvas.FillRect(RectF(Floor(r.Left) + 0.5, r.Top, Round(r.Left + tw) + 1.5, r.Bottom), 0, 0, AllCorners, AbsoluteOpacity);
          ACanvas.Fill.Color := clBlack;
        end;

        if ((el as TTextElement).Color <> clNone) and not bkgdrawn then
        begin
          ACanvas.Fill.Color := (el as TTextElement).Color;
          ACanvas.FillRect(RectF(Floor(r.Left) + 0.5, r.Top, Round(r.Left + tw) + 1.5, r.Bottom), 0, 0, AllCorners, AbsoluteOpacity);
        end;

        if el.URL = '' then
          ACanvas.Fill.Color := (el as TTextElement).TextColor
        else
          ACanvas.Fill.Color := URLColor;

        DrawTextN(ACanvas, preValue, r, 0);

        if iserr then
          DrawErrorLine(ACanvas, Round(r.Left), Round(tw), Round(r.Bottom), clRed);

        if (el as TTextElement).Line then
        begin
          ACanvas.Stroke.Color := clBlack;
          ACanvas.DrawLine(PointF(r.Left, r.Bottom), PointF(MaxLineWidth, r.Bottom), AbsoluteOpacity);
        end;

        r.Left := r.Left + tw;
      end;

      if IsForwardSelection then
      begin
        if (el = Selection.FromElement) and (CharIndex <= Selection.FromChar) then
          Selection.FromXY := Point(Round(r.Left), Round(r.Top));
      end
      else
      begin
        if (el = Selection.ToElement) and (CharIndex <= Selection.ToChar) then
          Selection.FromXY := Point(Round(r.Left), Round(r.Top));
      end;

      if Value <> '' then
      begin
        ACanvas.Fill.Color := SelectionColor;
        tw := CalculateText(ACanvas, Value).Width;
        ACanvas.FillRect(RectF(Floor(r.Left) + 0.5, r.Top, Round(r.Left + tw) + 1.5, r.Bottom), 0, 0, AllCorners, AbsoluteOpacity);
        ACanvas.Fill.Color := SelectionTextColor;

        DrawTextN(ACanvas, Value, r, 0);

        if iserr then
          DrawErrorLine(ACanvas, Round(r.Left), Round(tw), Round(r.Bottom), clRed);

        if (el as TTextElement).Line then
        begin
          ACanvas.Stroke.Color := clBlack;
          ACanvas.DrawLine(PointF(r.Left, r.Bottom), PointF(MaxLineWidth, r.Bottom), AbsoluteOpacity);
        end;

        r.Left := r.Left + tw;
      end;

      if IsForwardSelection then
      begin
        if (el = Selection.ToElement) and (CharIndex < Selection.ToChar) then
          Selection.ToXY := Point(Round(r.Left), Round(r.Bottom));
      end
      else
      begin
        if (el = Selection.FromElement) and (CharIndex < Selection.FromChar) then
          Selection.ToXY := Point(Round(r.Left), Round(r.Bottom));
      end;

      if postValue <> '' then
      begin
        tw := -1;
        bkgdrawn := False;
        if el.Highlight then
        begin
          bkgdrawn := True;
          c := ACanvas.Fill.Color;
          ACanvas.Fill.Color := clYellow;
          if tw = -1 then
            tw := CalculateText(ACanvas, postValue).Width;
          ACanvas.FillRect(RectF(Floor(r.Left) + 0.5, r.Top, Round(r.Left + tw) + 1.5, r.Bottom), 0, 0, AllCorners, AbsoluteOpacity);
          ACanvas.Fill.Color := c;
        end;

        if el.MergeRef <> '' then
        begin
          bkgdrawn := True;
          ACanvas.Fill.Color := clSilver;
          if tw = -1 then
            tw := CalculateText(ACanvas, postValue).Width;
          ACanvas.FillRect(RectF(Floor(r.Left) + 0.5, r.Top, Round(r.Left + tw) + 1.5, r.Bottom), 0, 0, AllCorners, AbsoluteOpacity);
          ACanvas.Fill.Color := clBlack;
        end;

        if ((el as TTextElement).Color <> clNone) and not bkgdrawn then
        begin
          ACanvas.Fill.Color := (el as TTextElement).Color;
          if tw = -1 then
            tw := CalculateText(ACanvas, postValue).Width;
          ACanvas.FillRect(RectF(Floor(r.Left) + 0.5, r.Top, Round(r.Left + tw) + 1.5, r.Bottom), 0, 0, AllCorners, AbsoluteOpacity)
        end;

        if el.URL = '' then
          ACanvas.Fill.Color := (el as TTextElement).TextColor
        else
          ACanvas.Fill.Color := URLColor;

        if (FImeString = '') or (FImeLine <> y) then
          DrawTextN(ACanvas, postValue, r, 0)
        else
        begin
          if (postValue = FImeString) then
            DrawTextN(ACanvas, postValue, r, 0)
          else
            DrawImeText(postValue, True);
        end;

        if iserr then
        begin
          if tw = -1 then
            tw := CalculateText(ACanvas, postValue).Width;
          DrawErrorLine(ACanvas, Round(r.Left), Round(tw), Round(r.Bottom), clRed);
        end;

        if (el as TTextElement).Line then
        begin
          ACanvas.Stroke.Color := clBlack;
          ACanvas.DrawLine(PointF(r.Left, r.Bottom), PointF(MaxLineWidth, r.Bottom), AbsoluteOpacity);
        end;
      end;
    end
    else
    begin
      tw := -1;
      bkgdrawn := False;

      if el.Highlight then
      begin
        bkgdrawn := True;
        c := ACanvas.Fill.Color;
        ACanvas.Fill.Color := clYellow;
        if tw = -1 then
          tw := CalculateText(ACanvas, AValue).Width;
        ACanvas.FillRect(RectF(Floor(r.Left) + 0.5, r.Top, Round(r.Left + tw) + 1.5, r.Bottom), 0, 0, AllCorners, AbsoluteOpacity);
        ACanvas.Fill.Color := c;
      end;

      if el.MergeRef <> '' then
      begin
        bkgdrawn := True;
        ACanvas.Fill.Color := clSilver;
        if tw = -1 then
          tw := CalculateText(ACanvas, AValue).Width;
        ACanvas.FillRect(RectF(Floor(r.Left) + 0.5, r.Top, Round(r.Left + tw) + 1.5, r.Bottom), 0, 0, AllCorners, AbsoluteOpacity);
        ACanvas.Fill.Color := clBlack;
      end;

      if ((el as TTextElement).Color <> clNone) and not bkgdrawn then
      begin
        ACanvas.Fill.Color := (el as TTextElement).Color;
        if tw = -1 then
          tw := CalculateText(ACanvas, AValue).Width;

        ACanvas.FillRect(RectF(Floor(r.Left) + 0.5, r.Top, Round(r.Left + tw) + 1.5, r.Bottom), 0, 0, AllCorners, AbsoluteOpacity)
      end;

      if el.URL = '' then
        ACanvas.Fill.Color := (el as TTextElement).TextColor
      else
        ACanvas.Fill.Color := URLColor;

//      if (FImeString <> '') and (AValue.Substring(FImeStartIndex, Length(FImeString)) = FImeString) then
//        DrawImeText(AValue, FImeLine = y)
//      else
        DrawTextN(ACanvas, AValue, r, 0);

      if iserr then
      begin
        if tw = -1 then
          tw := CalculateText(ACanvas, AValue).Width;
        DrawErrorLine(ACanvas, Round(r.Left), Round(tw), Round(r.Bottom), clRed);
      end;

      if (el as TTextElement).Line then
      begin
        ACanvas.Stroke.Color := clBlack;
        ACanvas.DrawLine(PointF(r.Left, r.Bottom), PointF(MaxLineWidth, r.Bottom), AbsoluteOpacity);
      end;
    end;
  end
  else
  begin
    if el = Selection.FromElement then
    begin
      Selection.FromXY := Point(x, y);
    end;

    if el = Selection.ToElement then
    begin
      Selection.ToXY := Point(x,y + LineHeight);
    end;
  end;

  if (el is TLineElement) then
  begin
    ACanvas.Stroke.Color := (el as TLineElement).Color;
    {$IFDEF DELPHITOKYO}
    ACanvas.Stroke.Thickness := (el as TLineElement).Width;
    ACanvas.Stroke.Dash := TStrokeDash((el as TLineElement).Style);
    {$ELSE}
    ACanvas.StrokeThickness := (el as TLineElement).Width;
    ACanvas.StrokeDash := TStrokeDash((el as TLineElement).Style);
    {$ENDIF}
    ACanvas.DrawLine(PointF(0, y + LineHeight div 2), PointF(MaxLineWidth, y + LineHeight div 2), AbsoluteOpacity);
  end;

  if (el is TCustomGraphicElement) then
  begin
    dpi := (el as TCustomGraphicElement).DPIratio;

    r := RectF(x,y,x + (el as TCustomGraphicElement).Size.cx * dpi, y + (el as TCustomGraphicElement).Size.cy * dpi);

    InflateRect(r, -PICTURE_MARGIN div 2, -PICTURE_MARGIN div 2);

    if el.Selected and not HideSelection and HasTextSelection then
    begin
      ACanvas.Fill.Color := SelectionColor;
      ACanvas.Stroke.Color := SelectionColor;
      ACanvas.FillRect(r, 0, 0, AllCorners, AbsoluteOpacity);
      ACanvas.DrawRect(r, 0, 0, AllCorners, AbsoluteOpacity);
    end;

    if dpi <> 1.0 then
    begin
      ACanvas.Fill.Color := clWhite;
      ACanvas.Stroke.Color := clWhite;
      ACanvas.FillRect(r, 0, 0, AllCorners, AbsoluteOpacity);
      ACanvas.DrawRect(r, 0, 0, AllCorners, AbsoluteOpacity);
    end;

    pic := nil;
    pw := 0;
    ph := 0;

//    if (el is TNamedPictureElement) then
//    begin
//      if (el is TEmoticonPictureElement) then
//        pic := GetPictureByName(Emoticons, (el as TNamedPictureElement).Name)
//      else
//        pic := GetPictureByName(BitmapContainer, (el as TNamedPictureElement).Name);
//
//      if not Assigned(pic) then
//      begin
//        ACanvas.Stroke.Color := clRed;
//        ACanvas.StrokeThickness := 1;
//        ACanvas.StrokeDash := TStrokeDash.Solid;
//        ACanvas.DrawLine(PointF(r.Left,r.Top), PointF(r.Right,r.Bottom), AbsoluteOpacity);
//        ACanvas.DrawLine(PointF(r.Right, r.Top), PointF(r.Left,r.Bottom), AbsoluteOpacity);
//      end
//      else
//      begin
//        (el as TNamedPictureElement).Width := pic.Width;
//        (el as TNamedPictureElement).Height := pic.Height;
//      end;
//    end;

    if (el is TPictureElement) then
    begin
      pic := (el as TPictureElement).Picture;
      ph := (el as TPictureElement).PictureHeight;
      pw := (el as TPictureElement).PictureWidth;
    end;

    if (el is TPictureElement) or (el is TNamedPictureElement) then
    begin
      if Assigned(pic) and not pic.IsEmpty then
      begin
        if (dpi <> 1.0) then
          ACanvas.DrawBitmap(pic, RectF(0, 0, pic.Width, pic.Height), r, AbsoluteOpacity)
        else
        begin
          if (pw > 0) and (ph > 0) then
            ACanvas.DrawBitmap(pic, RectF(0, 0, pic.Width, pic.Height), r, AbsoluteOpacity)
          else
            ACanvas.DrawBitmap(pic, RectF(0, 0, pic.Width, pic.Height), RectF(x + PICTURE_MARGIN div 2, y, x + PICTURE_MARGIN
              div 2 + pic.Width, y + pic.Height), AbsoluteOpacity)
        end;
      end;
    end;

    if (el is TGraphicElement) then
    begin
//      if Assigned(OnDrawGraphic) then
//        OnDrawGraphic(Self, ACanvas, r, (el as TGraphicElement).ID)
//      else
      begin
        ACanvas.Stroke.Color := clRed;
        {$IFDEF DELPHITOKYO}
        ACanvas.Stroke.Thickness := 1;
        ACanvas.Stroke.Dash := TStrokeDash.Solid;
        {$ELSE}
        ACanvas.StrokeThickness := 1;
        ACanvas.StrokeDash := TStrokeDash.Solid;
        {$ENDIF}
        ACanvas.DrawLine(PointF(r.Left,r.Top), PointF(r.Right,r.Bottom), AbsoluteOpacity);
        ACanvas.DrawLine(PointF(r.Right, r.Top), PointF(r.Left,r.Bottom), AbsoluteOpacity);
      end;
    end;

    if (el = Selected) then
      DrawSelection(ACanvas, Rect(Round(r.Left), Round(r.Top), Round(r.Right), Round(r.Bottom)));
  end;
end;
{$ENDIF}



procedure TAdvRichEditor.DrawErrorLine(ACanvas: TCanvas; x, w, y: integer; AColor: TColor);
var
  l,o: integer;
{$IFDEF FMXLIB}
  oo: integer;
{$ENDIF}
begin
  l := (x div 2) * 2;
  if (l mod 4)=0 then o := 2 else o := 0;

{$IFNDEF FMXLIB}
  ACanvas.Pen.Color := AColor;
  ACanvas.Pen.Width := 1;

  ACanvas.MoveTo(l,y + o - 1);

  while l < x + w do
  begin
    if o = 2 then o := 0 else o := 2;
    ACanvas.LineTo(l + 2,y + o - 1);
    Inc(l,2);
  end;
{$ENDIF}

{$IFDEF FMXLIB}
  ACanvas.Stroke.Color := AColor;

  {$IFDEF DELPHITOKYO}
  ACanvas.Stroke.Thickness := 1;
  {$ELSE}
  ACanvas.StrokeThickness := 1;
  {$ENDIF}

  while l < x + w do
  begin
    if o = 2 then
    begin
      o := 0;
      oo := 2;
    end
    else
    begin
      o := 2;
      oo := 0;
    end;

    ACanvas.DrawLine(PointF(int(l) + 0.5, int(y + oo - 1) + 0.5), PointF(int(l + 2 + 0.5),int(y + o - 1) + 0.5), AbsoluteOpacity);
    Inc(l,2);
  end;
{$ENDIF}
end;

procedure TAdvRichEditor.DrawLineBreak(ACanvas: TCanvas; x,y:integer; el: TREElement);
begin
{$IFNDEF FMXLIB}
  ACanvas.Brush.Color := SelectionColor;
  ACanvas.Pen.Color := ACanvas.Brush.Color;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Pen.Style := psSolid;

  ACanvas.Rectangle(x - 1, y, x + 1, y + el.LH);

  if IsForwardSelection then
  begin
    if Selection.FromElement = el then
      Selection.FromXY := el.XY;

    if Selection.ToElement = el then
      Selection.ToXY := Point(el.XY.X ,el.XY.Y + el.LH);
  end
  else
  begin
    if Selection.ToElement = el then
      Selection.FromXY := el.XY;

    if Selection.FromElement = el then
      Selection.ToXY := Point(el.XY.X ,el.XY.Y + el.LH);
  end;
{$ENDIF}
end;

function TAdvRichEditor.DrawGraphic(ACanvas: TCanvas; ARect: TRect; ID: string): boolean;
begin
  Result := Assigned(OnDrawGraphic);

  if Assigned(OnDrawGraphic) then
    OnDrawGraphic(Self, ACanvas, ARect, ID)
end;

procedure TAdvRichEditor.DrawMargin(ACanvas: TCanvas);
var
{$IFDEF FMXLIB}
  R: TRectF;
{$ENDIF}
{$IFNDEF FMXLIB}
  R: TRect;
{$ENDIF}
begin
{$IFNDEF FMXLIB}
  R := ClientRect;
  R.Bottom := PageMargin.Vertical;

  ACanvas.Brush.Color := PageMargin.Color;
  ACanvas.Pen.Color := PageMargin.Color;

  ACanvas.FillRect(r);

  R := ClientRect;
  R.Right := PageMargin.Horizontal;
  ACanvas.FillRect(r);

  R := ClientRect;
  R.Left := R.Right - PageMargin.Horizontal;
  ACanvas.FillRect(r);
{$ENDIF}

{$IFDEF FMXLIB}
  R := LocalRect;
  R.Bottom := PageMargin.Vertical;

  ACanvas.Fill.Kind := TBrushKind.Solid;
  ACanvas.Fill.Color := PageMargin.Color;
  ACanvas.FillRect(r, 0, 0, AllCorners, AbsoluteOpacity);

  R := LocalRect;
  R.Right := PageMargin.Horizontal;
  ACanvas.FillRect(r, 0, 0, AllCorners, AbsoluteOpacity);

  R := LocalRect;
  R.Left := R.Right - PageMargin.Horizontal;
  ACanvas.FillRect(r, 0, 0, AllCorners, AbsoluteOpacity);
{$ENDIF}
end;

procedure TAdvRichEditor.DrawSelection(ACanvas: TCanvas; r: TRect);
{$IFDEF FMXLIB}
var
  rc: TRectF;
  sz: Single;
{$ENDIF}
begin
{$IFNDEF FMXLIB}
  ACanvas.Brush.Color := GraphicSelection.Color;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Pen.Color := GraphicSelection.BorderColor;
  ACanvas.Pen.Style := psSolid;

  if GraphicSelection.Style = gsCircle then
  begin
    ACanvas.Ellipse(r.Left - 3,r.Top - 3, r.Left + 3, r.Top + 3);
    ACanvas.Ellipse(r.Left - 3,r.Bottom - 3, r.Left + 3, r.Bottom + 3);
    ACanvas.Ellipse(r.Right - 3,r.Top - 3, r.Right + 3, r.Top + 3);
    ACanvas.Ellipse(r.Right - 3,r.Bottom - 3, r.Right + 3, r.Bottom + 3);
  end
  else
  begin
    ACanvas.Rectangle(r.Left - 3,r.Top - 3, r.Left + 3, r.Top + 3);
    ACanvas.Rectangle(r.Left - 3,r.Bottom - 3, r.Left + 3, r.Bottom + 3);
    ACanvas.Rectangle(r.Right - 3,r.Top - 3, r.Right + 3, r.Top + 3);
    ACanvas.Rectangle(r.Right - 3,r.Bottom - 3, r.Right + 3, r.Bottom + 3);
  end;
{$ENDIF}

{$IFDEF FMXLIB}

  {$IFDEF FMXMOBILE}
  sz := 5;
  {$ELSE}
  sz := 3;
  {$ENDIF}

  rc := RectF(r.Left, r.Top, r.Right, r.Bottom);
  InflateRect(rc, -0.5, -0.5);
  ACanvas.Fill.Color := GraphicSelection.Color;
  ACanvas.Fill.Kind := TBrushKind.Solid;
  ACanvas.Stroke.Kind := TBrushKind.Solid;
  ACanvas.Stroke.Color := GraphicSelection.BorderColor;

  if GraphicSelection.Style = gsCircle then
  begin
    ACanvas.FillEllipse(rectF(rc.Left - sz,rc.Top - sz, rc.Left + sz, rc.Top + sz), 0.5);
    ACanvas.DrawEllipse(rectF(rc.Left - sz,rc.Top - sz, rc.Left + sz, rc.Top + sz), 0.5);
    ACanvas.FillEllipse(rectF(rc.Left - sz,rc.Bottom - sz, rc.Left + sz, rc.Bottom + sz), 0.5);
    ACanvas.DrawEllipse(rectF(rc.Left - sz,rc.Bottom - sz, rc.Left + sz, rc.Bottom + sz), 0.5);
    ACanvas.FillEllipse(rectF(rc.right - sz,rc.Top - sz, rc.right + sz, rc.Top + sz), 0.5);
    ACanvas.DrawEllipse(rectF(rc.right - sz,rc.Top - sz, rc.right + sz, rc.Top + sz), 0.5);
    ACanvas.FillEllipse(rectF(rc.right - sz,rc.Bottom - sz, rc.right + sz, rc.Bottom + sz), 0.5);
    ACanvas.DrawEllipse(rectF(rc.right - sz,rc.Bottom - sz, rc.right + sz, rc.Bottom + sz), 0.5);
  end
  else
  begin
    ACanvas.FillRect(RectF(rc.Left - sz,rc.Top - sz, rc.Left + sz, rc.Top + sz), 0, 0, AllCorners, 0.5);
    ACanvas.DrawRect(RectF(rc.Left - sz,rc.Top - sz, rc.Left + sz, rc.Top + sz), 0, 0, AllCorners, 0.5);
    ACanvas.FillRect(RectF(rc.Left - sz,rc.Bottom - sz, rc.Left + sz, rc.Bottom + sz), 0, 0, AllCorners, 0.5);
    ACanvas.DrawRect(RectF(rc.Left - sz,rc.Bottom - sz, rc.Left + sz, rc.Bottom + sz), 0, 0, AllCorners, 0.5);
    ACanvas.FillRect(RectF(rc.right - sz,rc.Top - sz, rc.right + sz, rc.Top + sz), 0, 0, AllCorners, 0.5);
    ACanvas.DrawRect(RectF(rc.right - sz,rc.Top - sz, rc.right + sz, rc.Top + sz), 0, 0, AllCorners, 0.5);
    ACanvas.FillRect(RectF(rc.right - sz,rc.Bottom - sz, rc.right + sz, rc.Bottom + sz), 0, 0, AllCorners, 0.5);
    ACanvas.DrawRect(RectF(rc.right - sz,rc.Bottom - sz, rc.right + sz, rc.Bottom + sz), 0, 0, AllCorners, 0.5);
  end;
{$ENDIF}
end;


function TAdvRichEditor.DrawTextN(ACanvas: TCanvas; s: string; r: TRectF;
  Flags: DWORD): integer;
var
  H: Single;
begin
  H := Round(ACanvas.TextHeight(TextForHeight));
  if (r.Height > H) then
    r.Top := r.Bottom - H;
{$IFDEF FMXLIB}
  ACanvas.FillText(r, s, False, 1, [], TTextAlign.Leading, TTextAlign.Center);
{$ENDIF}
  Result := Round(r.Right - r.Left);
end;

function TAdvRichEditor.DrawTextN(ACanvas: TCanvas; s: string; r: TRect;
  Flags: DWORD): integer;
begin
  {$IFDEF VCLLIB}
  Result := DrawText(ACanvas.Handle, PChar(s), Length(s), r, Flags);
  {$ENDIF}

  {$IFDEF LCLLIB}
  ACanvas.TextOut(r.Left, r.Top, s);
  Result := ACanvas.TextHeight(TextForHeight);
  {$ENDIF}

  {$IFDEF FMXLIB}
  Result := 0;
  {$ENDIF}
end;

procedure TAdvRichEditor.EndPaintBuffer;
begin
  {$IFDEF LCLLIB}

  {$IFNDEF MSWINDOWS}
  if BorderStyle = bsSingle then
  begin
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 1;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0,0,Width - 1,Height - 1);
  end;
  {$ENDIF}

  {$ENDIF}

  {$IFNDEF FMXLIB}
  Canvas.Draw(BorderSize, BorderSize, Buffer);
  {$ENDIF}

  {$IFDEF FMXLIB}
  if Assigned(Buffer) then
  begin
    Buffer.Canvas.EndScene;
    Canvas.DrawBitmap(Buffer, RectF(0, 0, Buffer.Width, Buffer.Height), RectF(0, 0, GetClientWidth, GetClientHeight) , 1, True);
  end;
  {$ENDIF}
end;

{$IFDEF VCLLIB}
function TAdvRichEditor.GetCharInWord(el: TREElement; s: string; X: integer): integer;
var
  mw: integer;
  nnfit: integer;
  sz: TSize;

begin
  if X < 2 then
    Result := 0
  else
  begin
    mw := X + Canvas.TextWidth('n') div 2;
    Canvas.Font.Assign(el.Font);
    Canvas.Font.Size := Round(Canvas.Font.Size * FZoomFactor);

    GetTextExtentExPoint(Canvas.Handle, PChar(s), Length(s), mw, @nnfit, nil, sz);
    Result := Max(0, nnfit);
  end;
end;
{$ENDIF}

{$IFNDEF VCLLIB}
function TAdvRichEditor.GetCharInWord(el: TREElement; s: string; X: integer): integer;
var
  su,m: string;
  i: Integer;
  sz: Double;
  r,rm: TRectF;
begin
  if X < 2 then
    Result := 0
  else
  begin
    Result := 0;
    if (el is TTextElement) then
    begin
      Canvas.Font.Assign((el as TTextElement).Font);

      i := 1;
      sz := 0;

      while sz < X do
      begin
        su := Copy(s,1,i);
        m := Copy(s,i, 1);
        r := CalculateText(Canvas, su);
        rm := CalculateText(Canvas, m);
        sz := r.Right + (rm.Right - rm.Left) / 2;
        if sz < X then
          inc(i);
      end;
      Result := i;
    end;
  end;
end;
{$ENDIF}

function TAdvRichEditor.GetDefaultFont: TFont;
begin
  Result := Font;
end;

function TAdvRichEditor.GetDefaultFontColor: TColor;
begin
  Result := GetFontColor;
end;

function TAdvRichEditor.GetLineHeight(AElement: TREElement): integer;
{$IFNDEF FMXLIB}
var
  R: TRect;
{$ENDIF}
{$IFDEF FMXLIB}
var
  R: TRectF;
{$ENDIF}
begin
  if not Assigned(AElement) then
  begin
    Canvas.Font.Assign(GetDefaultFont);
  end
  else
    Canvas.Font.Assign(AElement.Font);

  {$IFNDEF FMXLIB}
  R := Rect(0,0,1000,1000);
  Result := DrawTextN(Canvas, TextForHeight, R, DT_LEFT or DT_CALCRECT);
  {$ENDIF}

  {$IFDEF FMXLIB}
  R := RectF(0,0,1000,1000);
  Result := Round(CalculateTextR(Canvas, TextForHeight, R).Height);
  {$ENDIF}

  Result := Result + LineSpacing;
end;

function TAdvRichEditor.SetRange(ARange: TSize): boolean;
var
  sz: TSize;
begin
  sz := ARange;
  {$IFNDEF FMXLIB}
  if SingleLine then
    sz.cy := Height - 4;
  {$ENDIF}
  {$IFDEF FMXLIB}
  if SingleLine then
    sz.cy := Round(Height) - 4;
  {$ENDIF}

  Result := inherited SetRange(sz);
end;

procedure TAdvRichEditor.SetRichEditorPopup(AValue: TAdvRichEditorPopup);
begin
  FRichEditorPopup := AValue;
  if Assigned(FRichEditorPopup) then
    FRichEditorPopup.RichEditor := Self;
end;

function TAdvRichEditor.GetClientHeight: integer;
begin
  {$IFNDEF FMXLIB}
  if HandleAllocated then
    Result := ClientHeight - 3 * BorderSize - HScrollHeight
  else
    Result := Height;
  {$ENDIF}
  {$IFDEF FMXLIB}
  Result := Round(Height);
  {$ENDIF}
end;

function TAdvRichEditor.GetClientWidth: integer;
begin
  {$IFNDEF FMXLIB}
  if HandleAllocated then
    Result := ClientWidth - 3 * BorderSize - VScrollWidth
  else
    Result := Width;
  {$ENDIF}
  {$IFDEF FMXLIB}
  Result := Round(Width);
  {$ENDIF}
end;

{$IFDEF TMSPACK}
function TAdvRichEditor.GetPictureByName(Container: TGDIPPictureContainer; const AName: string): TGDIPPicture;
var
  st: TMemoryStream;
  ap: TAdvGDIPPicture;
begin
  Result := nil;

  if Assigned(Container) then
  begin
    ap := Container.FindPicture(AName);

    if Assigned(ap) then
    begin
      Result := FGDIPPicture;
      st := TMemoryStream.Create;
      try
        ap.SaveToStream(st);
        st.Position := 0;
        Result.LoadFromStream(st);
      finally
        st.Free;
      end;
    end;
  end;
end;
{$ENDIF}

function TAdvRichEditor.GetPictureSize(el: TREElement): TSize;
var
  pic: TGDIPPicture;
  sz: TSize;

begin
  sz.cx := 0;
  sz.cy := 0;

  if (el is TPictureElement) then
  begin
    if ((el as TPictureElement).PictureWidth > 0) and ((el as TPictureElement).PictureHeight > 0) then
    begin
      sz.cx := (el as TPictureElement).PictureWidth;
      sz.cy := (el as TPictureElement).PictureHeight;
    end
    else
    begin
      pic := (el as TPictureElement).Picture;
      {$IFNDEF FMXLIB}
      if Assigned(pic) and not pic.Empty then
      {$ENDIF}
      {$IFDEF FMXLIB}
      if Assigned(pic) and not pic.IsEmpty then
      {$ENDIF}
      begin
       sz.cx := pic.Width;
       sz.cy := pic.Height;
      end;
    end;
  end;

  sz.cx := Round(sz.cx * DPIratio);
  sz.cy := Round(sz.cy * DPIratio);

  Result := sz;
end;

function TAdvRichEditor.GetTextDescent(ACanvas: TCanvas; el: TREElement): integer;
{$IFNDEF FMXLIB}
var
  tm: TTextMetric;
{$ENDIF}
begin
  {$IFNDEF FMXLIB}
  tm.tmDescent := 0;
  GetTextMetrics(ACanvas.Handle, TM);
  Result := tm.tmDescent;
  {$ENDIF}
  {$IFDEF FMXLIB}
  Result := 2;
  {$ENDIF}
end;

function TAdvRichEditor.GetTextSize(ACanvas: TCanvas; el: TREElement; AValue: string): TTextSize;
begin
  {$IFNDEF FMXLIB}
  if not FIsPainting then
    if not HandleAllocated then
      Exit;

  if Assigned(el) then
    ACanvas.Font.Assign(el.Font)
  else
    ACanvas.Font.Assign(GetDefaultFont);

  if (el is TTextElement) then
  begin
    if (el as TTextElement).StyleTag = 1 then
      AValue := Uppercase(AValue)
    else
    if (el as TTextElement).StyleTag = 2 then
      AValue := Lowercase(AValue);
  end;

  Result.cx := Round(ACanvas.TextWidth(AValue) * FZoomFactor);

  if not EqualFont(FCacheFont, ACanvas.Font) then
  begin
    FCacheHeight := Round(ACanvas.TextHeight(TextForHeight) * FZoomFactor);
    FCacheFont.Assign(ACanvas.Font);
  end;

  Result.cy := FCacheHeight + LineSpacing;

  Result.sup := 0;
  Result.sub := 0;

  if Assigned(el) then
  begin
    if (el.Baseline = tbSubscript) then
      Result.sub := Round(Result.cy/4);

    if (el.Baseline = tbSuperScript) then
      Result.sup := Round(Result.cy/4);
  end;
  {$ENDIF}

  {$IFDEF FMXLIB}
  if not Assigned(ACanvas) then
    Exit;

  if Assigned(el) then
    ACanvas.Font.Assign(el.Font)
  else
    ACanvas.Font.Assign(GetDefaultFont);

  Result.cx := Round(CalculateText(ACanvas, AValue).Width);

  if not EqualFont(FCacheFont, ACanvas.Font) then
  begin
    FCacheHeight := Round(CalculateText(ACanvas, TextForHeight).Height);
    FCacheFont.Assign(ACanvas.Font);
  end;

  Result.cy := FCacheHeight + LineSpacing;

  Result.sup := 0;
  Result.sub := 0;

  if Assigned(el) then
  begin
    if (el.Baseline = tbSubscript) then
      Result.sub := Round(Result.cy/4);

    if (el.Baseline = tbSuperScript) then
      Result.sup := Round(Result.cy/4);
  end;
  {$ENDIF}
end;

function Hiwrd(L: DWORD): word;
begin
  Result := L shr 16;
end;

function LoWrd(L: DWORD): word;
begin
  Result := L AND $FFFF;
end;

function MakeWrd(b1,b2: integer): integer;
begin
  Result := b1 or b2 shl 8;
end;

function MakeLng(i1,i2: integer): integer;
begin
  Result := i1 or i2 shl 16;
end;

function TAdvRichEditor.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiwrd(vn))) + '.' + IntToStr(Lo(Hiwrd(vn))) + '.' +
    IntToStr(Hi(Lowrd(vn))) + '.' + IntToStr(Lo(Lowrd(vn)));
end;

function TAdvRichEditor.GetDocURL: string;
begin
  Result := TTMSFNCRichEditorDocURL;
end;

function TAdvRichEditor.GetTipsURL: string;
begin
  Result := TTMSFNCRichEditorTipsURL;
end;


function TAdvRichEditor.GetVersionNr: Integer;
begin
  Result := MakeLng(MakeWrd(BLD_VER, REL_VER), MakeWrd(MIN_VER, MAJ_VER));
end;

function TAdvRichEditor.GetVersionString: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiwrd(vn)))+'.'+IntToStr(Lo(Hiwrd(vn)))+'.'+IntToStr(Hi(Lowrd(vn)))+'.'+IntToStr(Lo(Lowrd(vn)))+' '+DATE_VER;
end;

function TAdvRichEditor.GetBulletChar(const AType: TBulletType): string;
begin
  Result := #0;

  {$IFNDEF FMXLIB}

  {$IFNDEF LCLLIB}
  case AType of
  btCircle: Result := #$9F;
  btSquare: Result := #$A7;
  btStar: Result := #$AB;
  btArrow: Result := #$E0;
  btTick: Result := #$FC;
  end;
  {$ENDIF}

  {$IFDEF LCLLIB}
  case AType of
  btCircle: Result := #$2022;
  btSquare: Result := #$25AA;
  btStar: Result := #$20F0;
  btArrow: Result := #$2192;
  btTick: Result := #$FC;
  end;
  {$ENDIF}
  {$ENDIF}

  {$IFDEF FMXLIB}
  {$IFDEF MSWINDOWS}
  case AType of
    btCircle: Result := Chr($9F);
    btSquare: Result := Chr($A7);
    btStar: Result := Chr($AB);
    btArrow: Result := Chr($E0);
    btTick: Result := Chr($FC);
  end;
  {$ENDIF}
  {$IFDEF MACOS}
  case AType of
    btCircle: Result := Chr(8226);
    btSquare: Result := Chr(9642);
    btStar: Result := Chr(9733);
    btArrow: Result := Chr(10142);
    btTick: Result := Chr(10004);
  end;
  {$ENDIF}
  {$IFDEF ANDROID}
  case AType of
    btCircle: Result := Chr(8226);
    btSquare: Result := Chr(9642);
    btStar: Result := Chr(9734);
    btArrow: Result := Chr(62);
    btTick: Result := Chr(45);
  end;
  {$ENDIF}
  {$ENDIF}
end;

procedure TAdvRichEditor.UpdateBulletFont(ACanvas: TCanvas);
begin
  {$IFDEF LCLLIB}
  ACanvas.Font.Name := 'Arial';
  {$ENDIF}

  {$IFDEF FMXLIB}
  {$IFDEF MSWINDOWS}
  ACanvas.Font.Family := 'Wingdings';
  {$ENDIF}
  {$IFDEF ANDROID}
  ACanvas.Font.Family := 'Roboto';
  {$ENDIF}
  {$IFDEF MACOS}
  ACanvas.Font.Family := 'Symbol';
  {$ENDIF}
  {$ENDIF}
end;

function TAdvRichEditor.GetBulletSize(el: TREElement; AValue: string): TSize;
begin
  if Assigned(el) then
    Canvas.Font.Assign(el.Font)
  else
    Canvas.Font.Assign(GetDefaultFont);

  SetFontName(Canvas.Font, 'Wingdings');

  {$IFNDEF FMXLIB}
  {$IFDEF LCLLIB}
  Result.cx := Canvas.TextWidth(UTF8Encode(AValue));
  {$ENDIF}
  {$IFNDEF LCLLIB}
  Result.cx := Canvas.TextWidth(AValue);
  {$ENDIF}
  {$ENDIF}
  {$IFDEF FMXLIB}
  Result.cx := Round(Canvas.TextWidth(AValue));
  {$ENDIF}

  if AValue = '' then
    AValue := TextForHeight;

  {$IFNDEF FMXLIB}
  Result.cy := Canvas.TextHeight(AValue);
  {$ENDIF}
  {$IFDEF FMXLIB}
  Result.cy := Round(Canvas.TextHeight(AValue));
  {$ENDIF}


  if (el is TBulletElement) then
    Result.cx := Result.cx + (el as TBulletElement).Spacing;
end;

{$IFDEF FNCLIB}
{$IFNDEF FMXLIB}
procedure TAdvRichEditor.SetAdaptToStyle(const Value: Boolean);
begin
  inherited;
{$IFDEF USEVCLSTYLES}
  InitVCLStyle(not Value);
{$ENDIF}
end;
{$ENDIF}
{$ENDIF}

{$IFDEF FMXLIB}
procedure TAdvRichEditor.InitStyle;
var
  c: TAlphaColor;
begin
  inherited;

  c := claNull;

  if TTMSFNCStyles.GetStyleSelectionFillColor(c) then
    SelectionColor := c;

  c := claNull;

  if TTMSFNCStyles.GetStyleAlternativeTextFontColor(c) then
    SelectionTextColor := c;

  if TTMSFNCStyles.GetStyleTextFontColor(c) then
  begin
    SetFontColor(c);
    Caret.Color := c;
  end;
end;

procedure TAdvRichEditor.ResetToDefaultStyle;
begin
  inherited;

  SelectionColor := claBlue;
  SelectionTextColor := claWhite;
  SetFontColor(claBlack);
  Caret.Color := claBlack;
end;
{$ENDIF}

{$IFDEF USEVCLSTYLES}
procedure TAdvRichEditor.InitVCLStyle(init: boolean);
var
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  clr: TColor;
begin
  {$IFDEF FNCLIB}
  if not AdaptToStyle then
    Exit;
  {$ENDIF}

  UseVCLStyles := False;

  LStyle := StyleServices;

  if LStyle.Enabled and (LStyle.Name <> 'Windows') then
  begin
    UseVCLStyles := True;


    LDetails := LStyle.GetElementDetails(tgCellNormal);
    LStyle.GetElementColor(LDetails, ecFillColor, clr);

    {$IFDEF DELPHIXE6_LVL}
    if seClient in StyleElements then
    {$ENDIF}
    Color := clr;

    LStyle.GetElementColor(LDetails, ecBorderColor, clr);
    {$IFDEF DELPHIXE6_LVL}
    if seBorder in StyleElements then
    {$ENDIF}
    BorderColor := clr;

    LStyle.GetElementColor(LStyle.GetElementDetails(tgFixedCellNormal), ecBorderColor, clr);

    if LStyle.GetElementColor(LStyle.GetElementDetails(teEditTextNormal), ecTextColor, clr) and (clr <> clNone) then
    begin
      {$IFDEF DELPHIXE6_LVL}
      if seFont in StyleElements then
      {$ENDIF}
      Font.Color := clr;
    end;

    if LStyle.GetElementColor(LStyle.GetElementDetails(teEditTextSelected), ecFilLColor, clr) and (clr <> clNone) then
    begin
      SelectionColor := clr;
    end
    else
      SelectionColor := clHighlight;

    if LStyle.GetElementColor(LStyle.GetElementDetails(teEditTextSelected), ecTextColor, clr) and (clr <> clNone) then
    begin
      SelectionTextColor := clr;
    end
    else
      SelectionTextColor := clHighlightText;
  end
  else
  begin
    if init then
    begin
      Color := clWindow;
      BorderColor := clGray;
      Font.Color := clBlack;
      SelectionColor := clHighlight;
      SelectionTextColor := clHighlightText;
    end;
  end;
end;
{$ENDIF}

procedure TAdvRichEditor.InitContextMenu;
var
  alignmnu: TMenuItem;

  {$IFNDEF FMXLIB}
  {$IFNDEF LCLLIB}
  procedure AddImageListImage(AName: string);
  var
    pngbmp: TPngImage;
    bmp: Graphics.TBitmap;
  begin
    pngbmp := TPNGImage.Create;
    bmp := Graphics.TBitmap.Create;
    try
      pngbmp.LoadFromResourceName(HInstance, AName);
      pngbmp.AssignTo(bmp);
      bmp.AlphaFormat := afIgnored;
      ImageList_Add(FImageList.Handle, bmp.Handle, 0);
    finally
      pngbmp.Free;
      bmp.Free;
    end;
  end;
  {$ENDIF}

  {$IFDEF LCLLIB}
  procedure AddImageListImage(AName: string);
  var
    pngbmp: TPicture;
    bmp: Graphics.TBitmap;
  begin
    pngbmp := TPicture.Create;
    bmp := Graphics.TBitmap.Create;
    try
      pngbmp.LoadFromResourceName(HInstance, AName);
      bmp.TransparentColor:= clBlack;
      bmp.Width := pngbmp.Graphic.Width;
      bmp.Height := pngbmp.Graphic.Height;
      bmp.Canvas.Brush.Color := clBlack;
      bmp.Canvas.FillRect(0,0,bmp.Width, bmp.height);
      bmp.TransparentMode:= tmAuto;
      bmp.Canvas.Draw(0,0,pngbmp.Graphic);
      FImageList.AddMasked(bmp,clBlack);
    finally
      pngbmp.Free;
      bmp.Free;
    end;
  end;
  {$ENDIF}
  {$ENDIF}

  function CreateMenuItem(ACaption: string; {%H-}AAction: TAction; AImageIndex: integer; {%H-}AResourceName: string; AMenuAction: TPopupMenuAction; AShortCut: TShortCut): TMenuItem;
  var
    mnu: TMenuItem;
  begin
    mnu := TMenuItem.Create(FPopupMenu);
    {$IFDEF FMXLIB}
    mnu.Text := ACaption;
    if AResourceName <> '' then
      mnu.Bitmap.LoadFromResource(AResourceName);
    {$ENDIF}
    {$IFNDEF FMXLIB}
    mnu.Caption := ACaption;
    mnu.ImageIndex := AImageIndex;
    {$ENDIF}
    {$IFNDEF FNCLIB}
    mnu.Action := AAction;
    {$ENDIF}

    {$IFDEF FNCLIB}
    mnu.OnClick := DoPopupMenu;
    {$ENDIF}

    mnu.ShortCut := AShortCut;
    mnu.Tag := integer(AMenuAction);

    Result := mnu;
  end;

  function AddMenuItem(AMenu: TMenuItem; ACaption: string; AAction: TAction; AImageIndex: integer; AResourceName: string; AMenuAction: TPopupMenuAction; AShortCut: TShortCut): TMenuItem;
  begin
    Result := CreateMenuItem(ACaption, AAction, AImageIndex, AResourceName, AMenuAction, AShortCut);
    {$IFDEF FMXLIB}
    AMenu.AddObject(Result);
    {$ENDIF}
    {$IFNDEF FMXLIB}
    AMenu.Add(Result);
    {$ENDIF}
  end;

  function AddPopupItem(AMenu: TPopupMenu; ACaption: string; AAction: TAction; AImageIndex: integer; AResourceName: string; AMenuAction: TPopupMenuAction; AShortCut: TShortCut): TMenuItem;
  begin
    Result := CreateMenuItem(ACaption, AAction, AImageIndex, AResourceName, AMenuAction, AShortCut);
    {$IFDEF FMXLIB}
    AMenu.AddObject(Result);
    {$ENDIF}
    {$IFNDEF FMXLIB}
    AMenu.Items.Add(Result);
    {$ENDIF}
  end;


begin
  {$IFNDEF FMXLIB}
  FImageList.Masked := false;
  {$IFNDEF LCLLIB}
  FImageList.ColorDepth := cd32bit;
  {$ENDIF}
  {$IFDEF LCLLIB}
  FImageList.DrawingStyle := dsTransparent;
  {$ENDIF}

  AddImageListImage(TBRESNAMECUT);
  AddImageListImage(TBRESNAMECOPY);
  AddImageListImage(TBRESNAMEPASTE);

  FPopupMenu.Images := FImageList;
  {$ENDIF}

  {$IFDEF FNCLIB}
  FPopupMenu.OnPopup := DoPopupOpen;
  {$ENDIF}

  AddPopupItem(FPopupMenu, SREClear,FClearAction,-1,'',pmClear, ShortCut(KEY_DELETE, [ssCtrl]));
  AddPopupItem(FPopupMenu, '-',nil,-1,'',pmNone, 0);
  AddPopupItem(FPopupMenu, SRECut,FCutAction,0,TBRESNAMECUT,pmCut, ShortCut(KEY_X, [ssCtrl]));
  AddPopupItem(FPopupMenu, SRECopy,FCopyAction,1,TBRESNAMECOPY,pmCopy, ShortCut(KEY_C, [ssCtrl]));
  AddPopupItem(FPopupMenu, SREPaste,FPasteAction,2,TBRESNAMEPASTE,pmPaste, ShortCut(KEY_V, [ssCtrl]));
  AddPopupItem(FPopupMenu, '-',nil,-1,'',pmNone, 0);
  alignmnu := AddPopupItem(FPopupMenu, SREAlign,nil, -1,'',pmNone, 0);

  AddMenuItem(alignmnu, SRELeft, FLeftAction, -1,'',pmAlignLeft, ShortCut(KEY_L, [ssCtrl]));
  AddMenuItem(alignmnu, SRECenter, FCenterAction, -1,'',pmAlignCenter, ShortCut(KEY_E, [ssCtrl]));
  AddMenuItem(alignmnu, SRERight, FRightAction, -1,'',pmAlignRight, ShortCut(KEY_R, [ssCtrl]));

  {$IFDEF FMXLIB}
  PopupMenu := FPopupMenu;
  {$ENDIF}
end;

procedure TAdvRichEditor.InitializePictureSize(
  NamedPictureElement: TNamedPictureElement);
{$IFDEF TMSPACK}
var
  pic: TGDIPPicture;
{$ENDIF}
begin
  {$IFDEF TMSPACK}
  pic := GetPictureByName(Emoticons, NamedPictureElement.Name);

  if Assigned(pic) then
  begin
    NamedPictureElement.Width := pic.Width;
    NamedPictureElement.Height := pic.Height;
  end
  else
  begin
    NamedPictureElement.Width := 0;
    NamedPictureElement.Height := 0;
  end;
  {$ENDIF}
end;

procedure TAdvRichEditor.KeyDownN(var Key: Word; Shift: TShiftState);
var
  AState: TCharCommandState;
  {$IFDEF FMXLIB}
  AType: TBulletType;
  AIndex, AIndent: integer;
  {$ENDIF}
begin
  if ReadOnly then
    Exit;

  EnsureCaret;

  FDoCaret := true;
  FCaretTimer.Enabled := false;

  AState := ccNormal;

  if (ssShift in Shift) and not (ssCtrl in Shift) then
    AState := ccShift;

  if (ssShift in Shift) and (ssCtrl in Shift) then
    AState := ccCtrlShift;

  if not (ssShift in Shift) and (ssCtrl in Shift) and not (ssAlt in Shift) then
    AState := ccCtrl;

  if (Key in [KEY_DELETE,KEY_LEFT,KEY_RIGHT,KEY_PRIOR,KEY_NEXT]) then
  begin
    CharCommand(Key, AState);
    {$IFNDEF VCLLIB}
    Key := 0;
    {$ENDIF}
  end;

  if (Key in [VK_A,VK_C,VK_V,VK_X,VK_Z,VK_Y,VK_B,VK_I,VK_U,VK_E,VK_R,VK_L]) and (AState = ccCtrl) then
  begin
    CharCommand(Key, AState);
    {$IFNDEF VCLLIB}
    Key := 0;
    {$ENDIF}
  end;

  if (Key in [KEY_UP,KEY_DOWN,KEY_HOME,KEY_END,KEY_INSERT]) then
  begin
    CharCommand(Key, AState);
  end;

  {$IFDEF FMXLIB}
  if (Key = KEY_RETURN) then
  begin
    if not (ssShift in Shift) and IsCaretInBulletList(AType, AIndex, AIndent) then
      InsertLineBreakAndBullet(AType, AIndent)
    else
      InsertLineBreak(ssShift in Shift);
  end;

  if (Key = KEY_BACK) then
  begin
    Backspace;
  end
  {$ENDIF}
end;

{$IFDEF LCLLIB}
procedure TAdvRichEditor.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  AType: TBulletType;
  AIndex, AIndent: integer;
  IsShift: boolean;
  us: utf8string;
  s: string;
  Key: TUTF8Char;
begin
  EnsureCaret;

  if ReadOnly then
    Exit;

  Key := UTF8Key;
  us := Key;
  s := UTF8Encode(us);

  if (Key = #13) and not SingleLine then
  begin
    AIndent := 0;
    AIndex := 0;
    AType := btNone;

    IsShift := GetKeyState(VK_SHIFT) and $8000 = $8000;
    if not IsShift and IsCaretInBulletList(AType, AIndex, AIndent) then
      InsertLineBreakAndBullet(AType, AIndent)
    else
      InsertLineBreak(IsShift);
    DoChanged;
  end
  else
  if Key = #32 then
  begin
    InsertChar(s);
  end
  else
  if Key = #8 then
  begin
    Backspace;
  end
  else
  if Key = #9 then
  begin
    InsertTab;
  end
  else
  if Key = #$16 then
  begin
    // Ctrl-V
  end
  else
  if Key = #$18 then
  begin
    // Ctrl-X
  end
  else
  if Key = #$1A then
  begin
    // Ctrl-X
  end
  else
  if (Key < #20) then
  begin
    // Ctrl-A, Ctrl-B, Ctrl-C, Ctrl-I, Ctrl-U
  end
  else
    InsertChar(s);
end;
{$ENDIF}

procedure TAdvRichEditor.KeyPressN(var Key: Char);
var
{$IFNDEF FMXLIB}
  AType: TBulletType;
  AIndex, AIndent: integer;
{$ENDIF}
  IsShift: boolean;
begin
  {$IFDEF LCLLIB}
  Exit;
  {$ENDIF}

  EnsureCaret;

  if ReadOnly then
    Exit;

  {$IFDEF FMXLIB}
  IsShift := false;
  {$ENDIF}


  if (Key = #13) and not SingleLine then
  begin
    {$IFNDEF FMXLIB}
    AIndent := 0;
    AIndex := 0;
    AType := btNone;

    IsShift := GetKeyState(VK_SHIFT) and $8000 = $8000;
    if not IsShift and IsCaretInBulletList(AType, AIndex, AIndent) then
      InsertLineBreakAndBullet(AType, AIndent)
    else
    {$ENDIF}
      InsertLineBreak(IsShift);
    DoChanged;
  end
  else
  if Key = #32 then
  begin
    InsertChar(Key);
  end
  else
  if Key = #8 then
  begin
    Backspace;
  end
  else
  if (Key = #9) and WantTab then
  begin
    InsertTab;
  end
  else
  if Key = #$16 then
  begin
    // Ctrl-V
  end
  else
  if Key = #$18 then
  begin
    // Ctrl-X
  end
  else
  if Key = #$1A then
  begin
    // Ctrl-X
  end
  else
  if (Ord(Key) < $20) then
  begin
    // Ctrl-A, Ctrl-B, Ctrl-C, Ctrl-I, Ctrl-U
  end
  else
    InsertChar(Key);
end;

procedure TAdvRichEditor.KeyUpN(var Key: Word; Shift: TShiftState);
begin
  FCaretTimer.Enabled := true;
end;

procedure TAdvRichEditor.MouseDownN(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CurCaretEl,el: TREElement;
  CurCaretChar: integer;
  SP: TPoint;
  hasElem: boolean;
  IsSel,CanSelect: boolean;
  URLKey: boolean;
  r: TRect;
begin
  if not Enabled then
    Exit;

  if (Button <> TMouseButton.mbLeft) then
    Exit;

  FClickOnSel := false;
  CurCaretEl := Caret.Element;
  CurCaretChar := Caret.CharIndex;

  if not FDoImageResize then
  begin
    FDoImageResize := (Cursor = crSizeNESW) or (Cursor = crSizeNWSE);
    if FDoImageResize then
      PushContext;
  end;

  {$IFNDEF FNCLIB}
  if Assigned(PopupToolBar) and PopupToolBar.Visible then
    PopupToolBar.Hide;
  {$ENDIF}
  {$IFDEF FNCLIB}
  if Assigned(ToolBarPopup) and ToolBarPopup.Activated and not ToolBarPopup.DropDownActive then
    ToolBarPopup.Hide;
  {$ENDIF}

  FMouseDown := true;

  SetFocus;

  SP := TopLeft;
  FDownPos := Point(X,Y);
  FDownXY := Point(X,Y);
  IsSel := false;

  if not (ssAlt in Shift) then
  begin
    XYToCaret(X,Y, True, IsSel);

    if IsSel and FAllowSelect and not FDoImageResize then
    begin
      FClickSelXY := Point(X,Y);
      FClickOnSel := true;

      if (Caret.Element is TCustomGraphicElement) then
      begin
        el := Caret.Element;

        r := Rect(el.XY.X - 3 - SP.X, el.XY.Y - 3 , el.XYE.X + 3 - SP.X, el.XYE.Y + 3 );

        if not PtInRect(r, Point(X,Y)) then
        begin
          Selected := nil;
          Selection.FromElement := nil;
          Selection.ToElement := nil;
          el.Selected := false;
          Caret.Element.Selected := false;
          Refresh;
          Exit;
        end
        else
        begin
          CanSelect := true;
          DoCanSelectGraphic(Caret.Element, CanSelect);

          if CanSelect then
          begin
            Selected := Caret.Element;
            Selection.FromElement := Caret.Element;
            Selection.ToElement := Caret.Element;
            Refresh;
            DoSelectionChanged;
          end;

          DoClickGraphic(Caret.Element);

          Exit;
        end;
      end;

    end;
  end;

  XYToCaret(X,Y,False,IsSel);

  if Assigned(Caret.Element) and (Caret.Element.URL <> '') and (URLOpen <> uoNone) then
  begin
    URLKey := false;

    case URLOpen of
    uoCtrl: URLKey := ssCtrl in Shift;
    uoAlt: URLKey := ssAlt in Shift;
    uoClick: URLKey := true;
    end;

    if (URLKey or not FAllowSelect) then
    begin
      DoClickHyperlink(Caret.Element.URL, Caret.Element.Text);
    end;
  end;

  if not FAllowSelect then
  begin
    FMouseDown := false;
    Exit;
  end;

  {$IFDEF VCLLIB}
  if not Dragging then
  {$ENDIF}
    SetCaptureN;

  FDoCaret := true;
  FCaretTimer.Enabled := false;

  SP := TopLeft;

  hasElem := XYToElement(X + sp.X,Y + sp.Y,el);

  Selected := nil;

  if hasElem and (el is TTabElement) then
  begin
    CanSelect := true;
    if CanSelect then
    begin
      Selected := el;
      Selection.FromElement := el;
      Selection.ToElement := el;
      Refresh;
      DoSelectionChanged;
    end;
  end;

  if hasElem and (el is TCustomGraphicElement) then
  begin
    CanSelect := true;
    DoCanSelectGraphic(el, CanSelect);

    FDownSize := (el as TCustomGraphicElement).Size;

    if CanSelect then
    begin
      Selected := el;
      Selection.FromElement := el;
      Selection.ToElement := el;
      Refresh;
      DoSelectionChanged;
    end;

    DoClickGraphic(el);
  end
  else
  if (ssShift in Shift) then
  begin
    Selection.FromElement := CurCaretEl;
    Selection.FromChar := CurCaretChar;
    Selection.ToElement := Caret.Element;
    Selection.ToChar := Caret.CharIndex;

    DoSelectionChanged;
  end;
end;

procedure TAdvRichEditor.MouseMoveN(Shift: TShiftState; X, Y: Integer);
var
  el,nel: TREElement;
  idx: integer;
  dx,dy: integer;
  SP: TPoint;
  hasElem, CanSize: boolean;
  r: TRect;
  inarea: boolean;
  CX, CY: integer;
  ar: double;
{$IFNDEF FNCLIB}
  rtf,txt: string;
  ms: TMemoryStream;
  dwEffects: Integer;
{$ENDIF}
  graphicresize: boolean;
  mouseinsel: boolean;
{$IFDEF FMXLIB}
  PT: TPointF;
{$ENDIF}
{$IFNDEF FMXLIB}
  PT: TPoint;
{$ENDIF}
  CtrlWidth: integer;
  CtrlHeight: integer;

begin
  SP := TopLeft;

  CtrlWidth := Round(Width);
  CtrlHeight := Round(Height);

  inarea := (x >= PageMargin.Horizontal) and (y >= PageMargin.Vertical) and (x <= CtrlWidth - PageMargin.Horizontal);

  if x < PageMargin.Horizontal then
    x := PageMargin.Horizontal + 3;
  if y < PageMargin.Vertical then
    y := PageMargin.Vertical + 3;
  if x > CtrlWidth - PageMargin.Horizontal then
    x := CtrlWidth - PageMargin.Horizontal;
  if y > CtrlHeight then
    y := CtrlHeight;

  el := nil;

  hasElem := XYToElement(X + sp.X,Y + sp.Y,el) or (Assigned(Selected));

  {$IFNDEF FMXLIB}
  if hasElem and ShowHint then
  begin
    if (FLastHint <> el.Hint) and (el.Hint <> '') then
    begin
      FLastHint := el.Hint;
      Application.CancelHint;
    end;
  end;
  {$ENDIF}

  {$IFNDEF FNCLIB}
  if HasSelection and not FMouseDown and
     Assigned(Selection.FromElement) and Assigned(Selection.ToElement) and
     Assigned(PopupToolBar) then
  begin
    {$IFNDEF FMXLIB}
    mouseinsel := false;

    if (y >= Selection.FromXY.Y) and (y <= Selection.ToXY.Y) then
    begin
      mouseinsel := (x >= Selection.FromXY.X) and (x <= Selection.ToXY.X);
    end;

    if not PopupToolBar.Visible and mouseinsel then
    begin
      PT := ClientToScreen(Point(Selection.FromXY.X, Selection.FromXY.Y - 44));
      PopupToolBar.RichEditor := Self;
      PopupToolBar.Show(PT);
    end;

    if PopupToolBar.Visible and not mouseinsel then
    begin
      if not PopupToolBar.MouseInPopup(ClientToScreen(Point(X,Y))) then
      PopupToolBar.Hide;
    end;
    {$ENDIF}
  end;
  {$ENDIF}

  {$IFDEF FNCLIB}
  if HasSelection and not FMouseDown and
     Assigned(Selection.FromElement) and Assigned(Selection.ToElement) and
     Assigned(ToolBarPopup) then
  begin
    mouseinsel := false;

    if (y >= Selection.FromXY.Y) and (y <= Selection.ToXY.Y) then
    begin
      mouseinsel := (x >= Selection.FromXY.X) and (x <= Selection.ToXY.X);
    end;

    if not ToolBarPopup.Activated and mouseinsel then
    begin
      ToolBarPopup.PlacementControl := Self;
      ToolBarPopup.Placement := ppAbsolute;
      ToolBarPopup.RichEditor := Self;
      ToolBarPopup.Activate;
      {$IFDEF CMNLIB}
      PT := ClientToScreen(Point(Selection.FromXY.X, Selection.FromXY.Y - ToolBarPopup.ToolBar.Height));
      {$ENDIF}
      {$IFDEF FMXLIB}
      PT := LocalToScreen(PointF(Selection.FromXY.X, Selection.FromXY.Y - ToolBarPopup.ToolBar.Height));
      {$ENDIF}
      ToolBarPopup.PlacementRectangle.Left := pt.X;
      ToolBarPopup.PlacementRectangle.Top := pt.Y;
      ToolBarPopup.PlacementRectangle.Right := pt.X;
      ToolBarPopup.PlacementRectangle.Bottom := pt.Y;
    end;

    if ToolBarPopup.Activated and not ToolBarPopup.DropDownActive and not mouseinsel then
    begin
      {$IFDEF CMNLIB}
      PT := ClientToScreen(Types.Point(X, Y));
      {$ENDIF}
      {$IFDEF FMXLIB}
      PT := LocalToScreen(PointF(X, Y));
      {$ENDIF}

      if not ToolBarPopup.PointInPopup(PointF(PT.X, PT.Y)) then
        ToolBarPopup.Deactivate;
    end;

  end;
  {$ENDIF}

  graphicresize := Assigned(Selected) and (Selected is TCustomGraphicElement) and FDoImageResize;

  if FMouseDown and FClickOnSel and not graphicresize and not ReadOnly then
  begin
    if ( (Abs(X - FClickSelXY.X) > 4) or (Abs(Y - FClickSelXY.Y) > 4)) then
    begin
      {$IFNDEF FNCLIB}
      FDropSource := TAdvRichEditorDropSource.Create(Self);

      rtf := GetContentAsRTF(true);
      txt := SelectedText;
      if (txt = '') and ElementsSelected then // bitmap handling when no text is involved causes problem
        txt := '*';

      ms := TMemoryStream.Create;

      try
        SaveSelectionToStream(ms);
        ms.Position := 0;
        FInternalDD := true;

        if not (StartTextDoDragDrop(FDropSource,txt,rtf,ms,DROPEFFECT_COPY or DROPEFFECT_MOVE,dwEffects) = DRAGDROP_S_CANCEL) then
        begin
          DragCaret.Element := nil;
        end;

      finally
        FInternalDD := false;
        ms.Free;
        FClickOnSel := false;
        FMouseDown := false;
      end;
      {$ENDIF}
    end;

    Exit;
  end;

  (*
  if (ssCtrl in Shift) then                                                                                    *
  begin
    XYToChar(X + SP.X,Y + sp.Y,el, CX, CY);
    DragCaret.XY := Point(CX - sp.X, CY- sp.Y);
    DragCaret.LH := 16;
    //TDebug.Write('drag caret: %d %d',[DragCaret.XY.X, DragCaret.XY.Y]);
    Repaint;
    Exit;
  end;
  *)

  if FMouseDown then
  begin
    if hasElem then
    begin
      if not FDoImageResize then
      begin
        if (el is TTextElement) then
        begin
          cx := 0;
          cy := 0;
          idx := XYToChar(X + SP.X,Y + sp.Y,el, CX, CY);

          if idx > -1 then
          begin
            Selection.ToElement := el;
            Selection.ToChar := idx;
            Caret.Element := el;
            Caret.CharIndex := idx;
            UpdateN;
          end;
        end;

        if (el is TCustomGraphicElement) or (el is TBulletStart) then
        begin
          Selection.ToElement := el;
          Selection.ToChar := 0;
          UpdateN;
        end;

        if (el is TTabElement) then
        begin
//          Selection.FromElement := el;
//          Selection.FromChar := Caret.CharIndex;
          Selection.ToElement := el;
          Selection.ToChar := Caret.CharIndex;
          UpdateN;
        end;

        if (el is TLineBreakElement) then
        begin
          if (Y + SP.Y > el.XY.Y + el.LH) then
          begin
            nel := NextElement(el);

            while Assigned(nel) and ((nel is TBulletStart) or (nel is TBulletElement)) do
              nel := NextElement(nel);

            if Assigned(nel) and not (nel is TLineBreakElement) then
            begin
              Selection.ToElement := nel;
              Selection.ToChar := 0;
            end
            else
              Caret.CharIndex := 1;
          end
          else
          begin
            Selection.ToElement := el;
            Selection.ToChar := 0;
          end;
          UpdateN;
        end;
      end;

      if (Selected is TCustomGraphicElement) then
      begin
        if FDoImageResize then
        begin
          dx := X - FDownXY.X;
          dy := Y - FDownXY.Y;

          case FImageResizeCorner of
          irTopLeft:
            begin
              dx := -dx;
              dy := -dy;
            end;
          irTopRight:
            begin
              dy := -dy;
            end;
          irBottomLeft:
            begin
              dx := -dx;
            end;
          end;

          if (dx <> 0) or (dy <> 0) then
          begin

            if (Selected is TPictureElement) then
            begin
              // sizing with aspect ratio
              if (ssCtrl in Shift) then
              begin
                ar := FDownSize.cx / FDownSize.cy;

                if dx > dy then
                begin
                  dy := Round(dx * ar);
                end
                else
                begin
                  dx := Round(dy * ar);
                end;
              end;

              if (FDownSize.cx  + dx > 0) then
                (Selected as TPictureElement).PictureWidth := FDownSize.cx  + dx;
              if (FDownSize.cy  + dy > 0) then
                (Selected as TPictureElement).PictureHeight := FDownSize.cy  + dy;
              UpdateN;
            end;

            if (Selected is TGraphicElement) then
            begin
              if ((Selected as TGraphicElement).Width  + dx > 0) then
                (Selected as TGraphicElement).Width := (Selected as TGraphicElement).Width  + dx;
              if ((Selected as TGraphicElement).Height  + dy > 0) then
                (Selected as TGraphicElement).Height := (Selected as TGraphicElement).Height  + dy;
            end;

            FDownPos := Point(X,Y);
            Refresh;
          end;
        end
        else
        begin
          Selection.ToElement := el;

          if X > el.XY.X + el.Size.cx div 2 then
            Selection.ToChar := 1
          else
            Selection.ToChar := 0;
          UpdateN;
        end;
      end;
    end;

    Caret.Element := Selection.ToElement;
    Caret.CharIndex := Selection.ToChar;
    Refresh;

    if (y < 20) and not FSingleLine then
      ScrollUp(20);

    if (Y > Height - 20) and not FSingleLine then
      ScrollDown(20);
  end
  else
  begin
    if hasElem then
    begin
      if (el is TCustomGraphicElement) and (el.Selected) then
      begin

        if (el.URL <> '') and (x < el.XYE.X) and (y < el.XYE.Y) then
          UpdateCursor(crHandPoint)
        else
        begin
          Cursor := crDefault;

          CanSize := true;
          DoCanSizeGraphic(el, CanSize);

          if CanSize then
          begin
            r := Rect(el.XY.X - 3 - SP.X, el.XY.Y - 3 , el.XY.X + 3 - SP.X, el.XY.Y + 3 );
            if PtInRect(r, Point(x + SP.X,y + SP.Y)) then
            begin
              UpdateCursor(crSizeNWSE);
              FImageResizeCorner := irTopLeft;
            end;

            r := Rect(el.XY.X + el.Size.cx - 6 - SP.X, el.XY.Y - 3 , el.XY.X + el.Size.cx - SP.X, el.XY.Y + 3 );
            if PtInRect(r, Point(x + SP.X,y + SP.Y)) then
            begin
              UpdateCursor(crSizeNESW);
              FImageResizeCorner := irTopRight;
            end;

            r := Rect(el.XY.X + el.Size.cx  - 6 - SP.X, el.XY.Y + el.Size.cy - 3 , el.XY.X + el.Size.cx - SP.X, el.XY.Y + el.Size.cy + 3 );
            if PtInRect(r, Point(x + SP.X,y + SP.Y)) then
            begin
              UpdateCursor(crSizeNWSE);
              FImageResizeCorner := irBottomRight;
            end;

            r := Rect(el.XY.X - 3 - SP.X, el.XY.Y + el.Size.cy - 3, el.XY.X + 3 - SP.X, el.XY.Y + el.Size.cy + 3 );
            if PtInRect(r, Point(x + SP.X,y + SP.Y)) then
            begin
              UpdateCursor(crSizeNESW);
              FImageResizeCorner := irBottomLeft;
            end;
          end;
        end;
      end
      else
      begin
        if (el.URL <> '') and (((x < el.XYE.X) and (y < el.XYE.Y)) or ((y < el.XYE.Y) and (y > el.XY.Y))) then
          UpdateCursor(crHandpoint)
        else
        begin
          RestoreCursor;
        end;
      end;
    end
    else
    begin
      RestoreCursor;
    end;
  end;

  if not inarea and FOrigIBeam then
  begin
    Cursor := crDefault;
  end;

  if not FAllowSelect and (Cursor = crIBeam) and FOrigIBeam then
    Cursor := crDefault;
end;

procedure TAdvRichEditor.MouseUpN(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  IsSel: boolean;
begin
  if FClickOnSel then
  begin
    IsSel := false;
    XYToCaret(X,Y,False,IsSel);
  end;

  if (Cursor <> crIBeam) and FOrigIBeam then
    Cursor := crIBeam;

  FDoImageResize := false;
  FMouseDown := false;
  FCaretTimer.Enabled := true;
  ReleaseCaptureN;
end;

procedure TAdvRichEditor.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
{$IFDEF TMSPACK}
  if (AComponent = FGDIPPictureContainer) and (AOperation = opRemove) then
    FGDIPPictureContainer := nil;
  if (AComponent = FEmoticons) and (AOperation = opRemove) then
    FEmoticons := nil;
{$ENDIF}

  if (AComponent = FRichEditorPopup) and (AOperation = opRemove) then
    FRichEditorPopup := nil;
end;

procedure TAdvRichEditor.Paint;
{$IFDEF FREEWARE}
var
  fw: string;
{$IFNDEF FMXLIB}
  r: TRect;
{$ENDIF}
{$ENDIF}
begin
  inherited;

  if FDoCaret and Focused then
    DrawCaret(Canvas);

  if Assigned(DragCaret.Element) then
    DrawDragCaret(Canvas);

  {$IFDEF FREEWARE}
  fw := ClassName  + trialversion + GetVersionString;

  {$IFNDEF FMXLIB}
  Canvas.Font.Color := clRed;
  Canvas.Brush.Style := bsClear;
  r.Left := 4;
  r.Top := Height - 20;
  r.Right := Width;
  r.Bottom := Height;
  DrawTextN(Canvas, fw, r, 0);
  {$ENDIF}

  {$IFDEF FMXLIB}
  Canvas.Fill.Color := clRed;
  DrawTextN(Canvas,fw,RectF(4, Height-20, Width, Height),0);
  {$ENDIF}

  {$ENDIF}
end;

procedure TAdvRichEditor.AddToolBar(AToolBar: ITMSRichEditorToolBar);
begin
  if Assigned(FToolBarList) then
    FToolBarList.Add(AToolBar);
end;

procedure TAdvRichEditor.RemoveToolBar(AToolBar: ITMSRichEditorToolBar);
begin
  if Assigned(FToolBarList) then
    FToolBarList.Remove(AToolBar);
end;

function TAdvRichEditor.PasteFromClipboard: string;
var
  didRTF, Allow: boolean;
  rtfvalue: string;
  pic: TPictureElement;
  sl: TStringList;
  i: integer;
{$IFNDEF FNCLIB}
  didHTML: boolean;
  htmlvalue: string;
  hDrop, hClip: THandle;
  count,c,ln,lp: integer;
  AFilename,AExt,su: string;
  bmp: TBitmap;
  bufptr: pointer;
  mstream: TStringStream;
{$ENDIF}
{$IFDEF FNCLIB}
  s,su: string;
  lp: integer;
  ts: TStringStream;
  clp: TTMSFNCClipboard;
  ms: TMemoryStream;
  bmp: TTMSFNCBitmap;
{$ENDIF}
begin
  didRTF := false;

  {$IFDEF FNCLIB}
  clp := TTMSFNCClipBoard.Create;

  try
    if clp.HasFormat(TTMSFNCClipBoardFormat.cfRichTextStream) then
    begin
      ms := clp.GetRichTextStream;
      if Assigned(ms) then
      begin
        PasteFromClipboardStream(ms);
        ms.Free;
        didRTF := true;
      end;
    end;

    if clp.HasFormat(TTMSFNCClipBoardFormat.cfRTF) and not didRTF then
    begin
      rtfvalue := clp.GetRTF;

      sl := TStringList.Create;
      sl.Text := rtfvalue;

      ts := TStringStream.Create;

      for i := 0 to sl.Count - 1  do
      begin
        s := sl.Strings[i];

        if (Length(s) > 0) and (CharInStr(s,1) <> '\') then
          ts.WriteString(#13);

        ts.WriteString(s);

        lp := Length(s);
        su := '';
        if lp >= 1 then
        begin
          su := Copy(s, lp ,1);
        end;

        if su = '\' then
          ts.WriteString('line');
      end;

      rtfvalue := ts.DataString;

      PushContext;

      if HasSelection then
        DeleteSelection;

      InsertAsRTF(rtfvalue);

      didRTF := true;
    end;

    if clp.HasFormat(TTMSFNCClipBoardFormat.cfBitmap) then
    begin
      PushContext;

      if HasSelection then
        DeleteSelection;

      bmp := clp.GetBitmap;
      {$IFDEF FMXLIB}
      if not bmp.IsEmpty then
      {$ENDIF}
      {$IFNDEF FMXLIB}
      if not bmp.Graphic.Empty then
      {$ENDIF}
      begin
        DoPasteBitmap(bmp, Allow);

        if Allow then
        begin
          PushContext;

          pic := TPictureElement.Create;
          pic.Picture.Assign(bmp);
          pic.FitToPage(GetClientWidth,GetClientHeight);

          InsertElementAtCaret(pic);
        end;
      end;
    end;

    if clp.HasFormat(TTMSFNCClipBoardFormat.cfText) and not didRTF then
    begin
      Result := clp.GetText;

      Allow := true;

      DoPasteText(Result, Allow);

      if Allow then
      begin
        if Result <> '' then
          PushContext;

        DeleteSelection;
        InsertMultiLineText(Result);
        HandleURLs;
      end;
    end;
  finally
    clp.Free;
  end;
  TidyElements;
  {$ENDIF}

  {$IFNDEF FNCLIB}
  didHTML := false;

  if Clipboard.HasFormat(ClipboardRTEFMT) and (cfRTE in ClipboardFormats) then
  begin
    PasteFormattedSelectionFromClipboard;
    TidyElements;
    Exit;
  end;

  if Clipboard.HasFormat(ClipboardRTFFMT) and (cfRTF in ClipboardFormats) then
  begin
    Clipboard.Open;

    HClip := Clipboard.GetAsHandle(ClipboardRTFFMT);
    bufptr := GlobalLock(HClip);
    if bufptr <> nil then
    begin
      try
        mstream := TStringStream.Create('');
        try
          mstream.WriteBuffer(bufptr^, GlobalSize(HClip));

          sl := TStringList.Create;
          sl.Text := UTF8ToString(RawByteString(mstream.DataString));

          mstream.Clear;
          try
            for i := 0 to sl.Count - 1 do
            begin
              rtfvalue := sl.Strings[i];
              if (Length(rtfvalue) > 0) and (CharInStr(rtfvalue,1) <> '\') then
                mstream.WriteString(#13);
              mstream.WriteString(rtfvalue);

              lp := Length(rtfvalue);
              su := '';
              if lp >= 2 then
                su := Copy(rtfvalue, lp - 1, 2);

              if su = ' \' then
                mstream.WriteString('line');
            end;
          finally
            sl.Free;
          end;

          PushContext;

          if HasSelection then
            DeleteSelection;

          InsertAsRTF(mstream.DataString);

          didRTF := true;
        finally
          mstream.Free;
        end;
      finally
        GlobalUnlock(HClip);
      end;
    end;
    Clipboard.Close;
    TidyElements;
    if didRTF then
      Exit;
  end;

  //if Clipboard.HasFormat(ClipboardJPEGFMT) then
  //begin
  //end;

  if Clipboard.HasFormat(CF_BITMAP) and (cfBMP in ClipboardFormats) then
  begin
    Clipboard.Open;
    bmp := Graphics.TBitmap.Create;
    bmp.Assign(Clipboard);
    Clipboard.Close;

    pic := TPictureElement.Create;
    pic.Picture.Assign(bmp);
    bmp.Free;

    if not pic.Picture.Empty then
      PushContext;

    Clipboard.Close;

    pic.FitToPage(Width,Height);

    InsertElementAtCaret(pic);
    Exit;
  end;


  if Clipboard.HasFormat(ClipboardHTMLFMT) and (cfHTML in ClipboardFormats) then
  begin
    Clipboard.Open;

    HClip := Clipboard.GetAsHandle(ClipboardHTMLFMT);

    bufptr := GlobalLock(HClip);
    if bufptr <> nil then
    begin
      try
        mstream := TStringStream.Create('');
        try
          mstream.WriteBuffer(bufptr^, GlobalSize(HClip));

          htmlvalue := UTF8ToString(RawByteString(mstream.DataString));
          c := pos('<HTML', Uppercase(htmlvalue));

          if c > 0 then
          begin
            didHTML := true;
            delete(htmlvalue, 1, c - 1);
            ParseHTML(htmlvalue);
          end;
        finally
          mstream.Free;
        end;
      finally
        GlobalUnlock(HClip);
      end;
    end;

    Clipboard.Close;
    TidyElements;
    if didHTML then
      Exit;
  end;

  if (Clipboard.HasFormat(CF_TEXT) or Clipboard.HasFormat(CF_UNICODETEXT)) and (cfText in ClipboardFormats) then
  begin
    Clipboard.Open;
    Result := Clipboard.AsText;
    Clipboard.Close;

    Allow := true;

    DoPasteText(Result, Allow);

    if Allow then
    begin
      if Result <> '' then
        PushContext;

      DeleteSelection;
      InsertMultiLineText(Result);
      HandleURLs;
    end;
    TidyElements;
    Exit;
  end;

  if Clipboard.HasFormat(CF_HDROP) and (cfFile in ClipboardFormats) then
  begin
    Clipboard.Open;

    hDrop := Clipboard.GetAsHandle(CF_HDROP);
    if hDrop <> 0 then
    begin
      count := DragQueryFile(hDrop, UINT(-1), nil, 0);

      for c := 0 to count - 1 do
      begin
        SetLength(AFileName, MAX_PATH);
        ln := DragQueryFile(hDrop, c, PChar(AFileName), MAX_PATH);
        SetLength(AFileName,ln);

        AExt := Uppercase(ExtractFileExt(AFileName));

        if (AExt = '.PNG') or (AExt = '.GIF') or (AExt = '.BMP') or (AExt = '.ICO') or (AExt = '.JPG') or (AExt = '.JPEG') then
        begin
          pic := TPictureElement.Create;
          pic.Picture.LoadFromFile(AFileName);
          InsertElementAtCaret(pic);
        end
        else
        begin
          sl := TStringList.Create;
          sl.LoadFromFile(AFileName);
          try
            InsertMultiLineText(sl.Text);
          finally
            sl.Free;
          end;
        end;
      end;
    end;

    Clipboard.Close;
  end;
  {$ENDIF}


  //if Clipboard.HasFormat(CF_PICTURE) then
  //begin
  //
  //end;
end;

procedure TAdvRichEditor.Print;
begin
{$IFNDEF FNCLIB}
  Printer.BeginDoc;

  FIsPrinting := true;

  PaintTo(Printer.Canvas, Printer.Canvas.ClipRect);

  FIsPrinting := false;

  Printer.EndDoc;
{$ENDIF}
end;

function TAdvRichEditor.CalcHeight(ACanvas: TCanvas; ARect: TRect): integer;
begin
  Result := PaintToInternal(ACanvas, ARect, true);
end;

procedure TAdvRichEditor.PaintTo(ACanvas: TCanvas; ARect: TRect);
begin
  PaintToInternal(ACanvas, ARect, false);
end;

function TAdvRichEditor.PaintToInternal(ACanvas: TCanvas; ARect: TRect; Calc: boolean): integer;
{$IFNDEF FNCLIB}
var
  FElementIndex,WordIndex,ToElementIndex,ToWordIndex,Descent,BaseLine,Indent: integer;
  FAlign: TAlignment;
  LW,LH,x,y: integer;
  MaxLineWidth: integer;
  MaxPageHeight: integer;
  marginx,marginy: integer;
  dpi: double;
  Sel: TSelection;
  r: TRect;
{$ENDIF}
begin
{$IFNDEF FNCLIB}
  FDoCaret := false;
  FCaretTimer.Enabled := false;
  FIsPainting := true;

  Sel := TSelection.Create;
  Sel.Assign(Selection);

  Selection.FromElement := nil;
  Selection.ToElement := nil;

  HideSelection := true;

  FElementIndex := 0;
  WordIndex := 0;
  Indent := 0;
  BaseLine := 0;

  dpi := GetDeviceCaps(ACanvas.Handle, LOGPIXELSX);
  DPIRatio := dpi / 96;

  MaxLineWidth := ARect.Right - ARect.Left;
  MaxPageHeight := ARect.Bottom - ARect.Top;

  marginx := ARect.Left;
  marginy := ARect.Top;

  if  FIsPrinting then
  begin
    marginx := MaxLineWidth div 20;
    marginy := MaxPageHeight div 20;
  end;

  MaxLineWidth := MaxLineWidth - 2 * marginx;
  MaxPageHeight := MaxPageHeight - 2 * marginy;

  r := Rect(marginx, marginy, marginx + maxlinewidth, marginy + maxpageheight);

  y := marginy;

  while (FElementIndex < Context.Content.Count) do
  begin
    CalcLine(ACanvas, -1, FElementIndex, WordIndex, MaxLineWidth, LW, LH, ToElementIndex, ToWordIndex, Descent, BaseLine, Indent, FAlign);

    x := OffsetX + Indent + ACanvas.ClipRect.Left + marginx;

    case FAlign of
      taRightJustify: x := marginx + OffsetX + Round(MaxLineWidth - LW - 1);
      taCenter: x := marginx + OffsetX + Round(MaxLineWidth - LW - 1) div 2;
    end;

    if Caret.NextLine then
    begin
      Caret.XY := Point(Caret.XY.X, y);
      Caret.LH := LH;
      Caret.NextLine := false;
    end;

    DrawLine(ACanvas, x, y, FElementIndex, WordIndex, MaxLineWidth + x, LW, LH, Descent, BaseLine, not Calc);

    if FIsPrinting then
      y := y + Round(LH)
    else
      y := y + Round(LH * DPIRatio);

    if (y > MaxPageHeight) and Clip and FIsPrinting then
    begin
      Printer.NewPage;
      y := marginy;
    end;

    FElementIndex := ToElementIndex;
    WordIndex := ToWordIndex;
  end;

  Result := y - marginy;

  Selection.Assign(Sel);
  Sel.Free;

  HideSelection := false;
  FIsPainting := false;
  DPIRatio := 1.0;

  Refresh;

  FDoCaret := true;
  FCaretTimer.Enabled := true;

{$ENDIF}
{$IFDEF FNCLIB}
  Result := 0;
{$ENDIF}
end;

procedure TAdvRichEditor.Refresh;
begin
  FDoCaret := true;
  {$IFNDEF FMXLIB}
  Invalidate;
  {$ENDIF}
  {$IFDEF FMXLIB}
  Repaint;
  {$ENDIF}
end;

procedure TAdvRichEditor.UpdateCursor(ACursor: TCursor);
begin
  if (Cursor <> ACursor) then
    FOrigCursor := Cursor;

  Cursor := ACursor;
end;

procedure TAdvRichEditor.RestoreCursor;
begin
  if FOrigCursor <> crNone then
    Cursor := FOrigCursor;
end;

procedure TAdvRichEditor.ReleaseCaptureN;
begin
  {$IFNDEF LCLLIB}
  ReleaseCapture;
  {$ENDIF}
  {$IFDEF LCLLIB}
  MouseCapture := False;
  {$ENDIF}
end;

procedure TAdvRichEditor.Resize;
begin
  inherited;
  UpdateSize;
  Refresh;
end;

procedure TAdvRichEditor.SetZoomFactor(const Value: Double);
begin
  SetFontName(FCacheFont, 'new');
  FZoomFactor := Value;
  UpdateSize;
  Refresh;
end;

procedure TAdvRichEditor.UpdateN;
begin
  {$IFDEF FMXLIB}
  Repaint;
  {$ENDIF}

  {$IFNDEF FMXLIB}
  Update;
  {$ENDIF}
end;

procedure TAdvRichEditor.SetCaptureN;
begin
  {$IFDEF FMXLIB}
  Capture;
  {$ENDIF}
  {$IFDEF VCLLIB}
  SetCapture(Handle);
  {$ENDIF}
  {$IFDEF LCLLIB}
  MouseCapture := True;
  {$ENDIF}
end;

procedure TAdvRichEditor.SetGraphicSelection(const Value: TGraphicSelection);
begin
  FGraphicSelection.Assign(Value);
end;

procedure TAdvRichEditor.SetVersion(const Value: string);
begin
  // readonly
end;

procedure TAdvRichEditor.UpdateSelection;
begin
  inherited;
end;

{$IFNDEF FMXLIB}
procedure TAdvRichEditor.WMDestroy(var Message: TWMDestroy);
begin
  inherited;

  {$IFNDEF FNCLIB}
  if FDropTargetAssigned then
  begin
    FDropTargetAssigned := false;
    RevokeDragDrop(Handle);
  end;
  {$ENDIF}
end;

procedure TAdvRichEditor.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS;

  if WantTab then
    Message.Result := Message.Result + DLGC_WANTTAB;
end;

procedure TAdvRichEditor.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
  DoDblClick(Message.XPos, Message.YPos);
end;
{$ENDIF}


{$IFDEF FNCLIB}
// ITMSFNCCustomEditor interface
procedure TAdvRichEditor.SetText(AValue: String);
begin
  Clear;
  InsertMultiLineText(AValue);
end;

procedure TAdvRichEditor.SetSelStart(AValue: Integer);
begin
  SelStart := AValue;
end;

procedure TAdvRichEditor.SetSelLength(AValue: Integer);
begin
  SelLength := AValue;
end;

function TAdvRichEditor.GetTextLength: Integer;
begin
  Result := Length(ContentAsPlainText);
end;

{$ENDIF}


{ TGraphicSelection }

procedure TGraphicSelection.Assign(Source: TPersistent);
begin
  if (Source is TGraphicSelection) then
  begin
    FColor := (Source as TGraphicSelection).Color;
    FBorderColor := (Source as TGraphicSelection).BorderColor;
    Style := (Source as TGraphicSelection).Style;
  end;
end;

constructor TGraphicSelection.Create;
begin
  inherited;
  FColor := clWhite;
  FBorderColor := clGray;
  FStyle := gsRect;
end;

{$IFNDEF FNCLIB}

{ TAdvRichEditorDropTarget }

constructor TAdvRichEditorDropTarget.Create(AEditor: TAdvRichEditor);
begin
  inherited Create;

  FRichEditor := AEditor;
end;

procedure TAdvRichEditorDropTarget.DragMouseLeave;
begin
  inherited;
end;

procedure TAdvRichEditorDropTarget.DragMouseMove(pt: TPoint; var Allow: Boolean;
  DropFormats: TDropFormats);
var
  IsSel: boolean;
  sp: TPoint;
begin
  pt := FRichEditor.ScreenToClient(pt);

  sp := FRichEditor.TopLeft;

  if (pt.y < 10) and (sp.Y > 0) then
  begin
    FRichEditor.TopLeft := Point(sp.X, sp.Y - FRichEditor.Caret.LH);
    Exit;
  end;

  if (pt.y > FRichEditor.Height - 10) then
  begin
    FRichEditor.TopLeft := Point(sp.X, sp.Y + FRichEditor.Caret.LH);
    Exit;
  end;

  FRichEditor.PushCaret;
  FRichEditor.PushSelection;

  try
    FRichEditor.XYToCaret(pt.X, pt.Y, False, IsSel);
    FRichEditor.Caret.XY := Point(-1,-1);
    FRichEditor.CalcCaretXY;
    FRichEditor.DragCaret.Assign(FRichEditor.Caret);
  finally
    FRichEditor.PopSelection;
    FRichEditor.PopCaret;
  end;
end;

procedure TAdvRichEditorDropTarget.DropBMP(pt: TPoint; bmp: Graphics.TBitmap; dwEffect: longint);
var
  IsSel: boolean;
  Allow: boolean;
begin
  Allow := FRichEditor.DoDropImage(TPicture(bmp));

  if Allow then
  begin
    if FRichEditor.FInternalDD and (dwEffect = DROPEFFECT_MOVE) then
    begin
      FRichEditor.DeleteSelection;
    end;

    FRichEditor.FInternalDD := false;

    pt := FRichEditor.ScreenToClient(pt);

    FRichEditor.XYToCaret(pt.X, pt.Y, False, IsSel);
    FRichEditor.InsertImage(TPicture(bmp),0,0);
  end;
end;

procedure TAdvRichEditorDropTarget.DropFiles(pt: TPoint; Files: TStrings; dwEffect: longint);
var
  fn,ext: string;
  IsSel, Allow: boolean;
  mStream: TStringStream;
begin
  inherited;

  if (Files.Count > 0) then
  begin
    fn := Files[0];

    ext := Uppercase(ExtractFileExt(fn));

    Allow := FRichEditor.DoDropFile(fn);

    if Allow then
    begin
      if (Ext = '.BMP') or (Ext = '.JPEG') or (Ext = '.JPG') or  (Ext = '.GIF') or (Ext = '.ICO') or (Ext = '.PNG') then
      begin
        pt := FRichEditor.ScreenToClient(pt);
        FRichEditor.XYToCaret(pt.X, pt.Y,False,IsSel);
        FRichEditor.InsertImage(fn,0,0);
        FRichEditor.SetFocus;
      end
      else
      if (Ext = '.RTE') then
        FRichEditor.LoadFromFile(fn)
      else
      if (Ext = '.TXT') then
        FRichEditor.LoadFromTextFile(fn)
      else
      if (Ext = '.RTF') then
      begin
        mStream := TStringStream.Create('');
        mStream.LoadFromFile(fn);
        try
          FRichEditor.InsertAsRTF(mstream.DataString);
        finally
          mStream.Free;
        end;
      end;
    end;

    FRichEditor.DragCaret.Element := nil;
    FRichEditor.UpdateN;
  end;
end;

procedure TAdvRichEditorDropTarget.DropRTF(pt: TPoint; s: string; dwEffect: longint);
begin
  inherited;
end;

procedure TAdvRichEditorDropTarget.DropStream(pt: TPoint; AStream: TMemoryStream; dwEffect: longint);
var
  mss: TStateSaver;
  ms: TMemoState;
  isSel: boolean;
begin
  if FRichEditor.FInternalDD and (dwEffect = DROPEFFECT_MOVE) then
    FRichEditor.DeleteSelection;

  FRichEditor.FInternalDD := false;

  FRichEditor.RegisterClasses;

  // read memo state first
  mss := TStateSaver.Create(nil);
  ms := TMemoState.Create;
  mss.SaveState := ms;
  try
    AStream.ReadComponent(mss);
    FRichEditor.LoadMemoState(ms, True);
  finally
    mss.Free;
    ms.Free;
  end;

  pt := FRichEditor.ScreenToClient(pt);
  FRichEditor.XYToCaret(pt.X, pt.Y, False, IsSel);

  FRichEditor.InsertFromStream(AStream, 1);
end;

procedure TAdvRichEditorDropTarget.DropText(pt: TPoint; s: string; dwEffect: longint);
var
  el: TREElement;
  SP: TPoint;
  hasElem, IsSel: boolean;
  t: string;
  idx, cx, cy: integer;
begin
  inherited;

  if FRichEditor.FInternalDD and (dwEffect = DROPEFFECT_MOVE) then
    FRichEditor.DeleteSelection;

  FRichEditor.FInternalDD := false;

  pt := FRichEditor.ScreenToClient(pt);
  SP := FRichEditor.TopLeft;

  if pt.x < FRichEditor.PageMargin.Horizontal then
    pt.x := FRichEditor.PageMargin.Horizontal + 3;
  if pt.y < FRichEditor.PageMargin.Vertical then
    pt.y := FRichEditor.PageMargin.Vertical + 3;
  if pt.x > FRichEditor.Width - FRichEditor.PageMargin.Horizontal then
    pt.x := FRichEditor.Width - FRichEditor.PageMargin.Horizontal;
  if pt.y > FRichEditor.Height then
    pt.y := FRichEditor.Height;

  hasElem := FRichEditor.XYToElement(pt.X + sp.X,pt.Y + sp.Y, el);

  if hasElem and (el is TTextElement) then
  begin
    t := (el as TTextElement).DisplayText;
    idx := FRichEditor.XYToChar(pt.X + SP.X,pt.Y + sp.Y,el, CX, CY);

    if idx > -1 then
    begin
      FRichEditor.Selection.ToElement := el;
      FRichEditor.Selection.ToChar := idx;
      FRichEditor.Caret.Element := el;
      FRichEditor.Caret.CharIndex := idx;
      FRichEditor.InsertText(s);
    end;
  end
  else
  begin
    pt := FRichEditor.ScreenToClient(pt);
    FRichEditor.XYToCaret(pt.X, pt.Y,False,IsSel);
    FRichEditor.InsertText(s);
  end;

  FRichEditor.DragCaret.Element := nil;
  FRichEditor.UpdateN;
end;

procedure TAdvRichEditorDropTarget.DropURL(pt: TPoint; s: string; dwEffect: longint);
var
  el: TREElement;
  IsSel: boolean;
begin
  pt := FRichEditor.ScreenToClient(pt);
  FRichEditor.XYToCaret(pt.X, pt.Y,False,IsSel);

  el := FRichEditor.InsertText(s);
  el.URL := s;
end;

{ TAdvRichEditorDropSource }

constructor TAdvRichEditorDropSource.Create(AEditor: TAdvRichEditor);
begin
  inherited Create;
  FRichEditor := AEditor;
end;

procedure TAdvRichEditorDropSource.CurrentEffect(dwEffect: Integer);
begin
  if dwEffect = DROPEFFECT_MOVE then
    FLastEffect := dwEffect;

  if dwEffect = DROPEFFECT_COPY then
    FLastEffect := dwEffect;
end;

procedure TAdvRichEditorDropSource.DragDropStop;
begin
  inherited;
end;

procedure TAdvRichEditorDropSource.QueryDrag;
begin
  inherited;
end;
{$ENDIF}


{$IFDEF FNCLIB}
{ TTMSFNCRichEditorCustomToolBarPopup }

constructor TTMSFNCRichEditorCustomToolBarPopup.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  if Assigned(AOwner) and (AOwner is TCustomForm) then
  begin
    for I := 0 to AOwner.ComponentCount - 1 do
    begin
      if (AOwner.Components[i] is TTMSFNCRichEditor) then
      begin
        RichEditor := AOwner.Components[i] as TTMSFNCRichEditor;
        RichEditor.ToolBarPopup := Self;
        Break;
      end;
    end;
  end;
end;

procedure TTMSFNCRichEditorCustomToolBarPopup.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = RichEditor) then
    RichEditor := nil;
end;
{$ENDIF}

{ TAdvRichEditorPopup }

procedure TAdvRichEditorPopup.Hide;
begin
  FVisible := false;
end;

procedure TAdvRichEditorPopup.Show(PT: TPoint);
begin
  FVisible := true;
end;


end.
