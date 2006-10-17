/* The following code was generated by JFlex 1.4.1 on 17.10.06 16:43 */

package org.jetbrains.plugins.scala.lang.lexer;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import java.util.*;
import java.lang.reflect.Field;
import org.jetbrains.annotations.NotNull;


/**
 * This class is a scanner generated by 
 * <a href="http://www.jflex.de/">JFlex</a> 1.4.1
 * on 17.10.06 16:43 from the specification file
 * <tt>scala.flex</tt>
 */
public class _ScalaLexer implements FlexLexer, ScalaTokenTypes {
  /** initial size of the lookahead buffer */
  private static final int ZZ_BUFFERSIZE = 16384;

  /** lexical states */
  public static final int YYINITIAL = 0;
  public static final int IN_XML_STATE = 4;
  public static final int IN_STRING_STATE = 3;
  public static final int IN_LINE_COMMENT_STATE = 2;
  public static final int IN_BLOCK_COMMENT_STATE = 1;

  /** 
   * Translates characters to character classes
   */
  private static final String ZZ_CMAP_PACKED = 
    "\11\0\1\32\1\56\1\0\1\32\1\55\22\0\1\32\1\77\1\54"+
    "\1\74\3\0\1\45\1\70\1\71\1\61\1\42\1\101\1\43\1\40"+
    "\1\60\1\34\1\36\1\50\1\36\1\36\1\51\1\36\1\36\1\52"+
    "\1\35\1\72\1\100\1\62\1\73\1\63\1\0\1\75\3\37\1\44"+
    "\1\41\1\44\5\47\1\33\16\47\1\64\1\46\1\65\1\0\1\53"+
    "\1\0\1\1\1\2\1\6\1\12\1\7\1\13\1\30\1\10\1\17"+
    "\1\25\1\27\1\11\1\21\1\16\1\14\1\22\1\31\1\5\1\3"+
    "\1\4\1\24\1\26\1\23\1\15\1\20\1\47\1\66\1\0\1\67"+
    "\1\76\u1faa\0\1\57\u01a8\0\1\0\ude2d\0";

  /** 
   * Translates characters to character classes
   */
  private static final char [] ZZ_CMAP = zzUnpackCMap(ZZ_CMAP_PACKED);

  /** 
   * Translates DFA states to action switch labels.
   */
  private static final int [] ZZ_ACTION = zzUnpackAction();

  private static final String ZZ_ACTION_PACKED_0 =
    "\3\0\1\1\1\0\1\2\21\3\1\4\2\5\1\6"+
    "\1\7\1\10\2\2\1\11\1\12\2\13\1\2\1\14"+
    "\2\2\1\15\1\16\1\17\1\20\1\21\1\22\1\23"+
    "\1\24\1\25\1\26\1\27\1\30\1\31\1\32\5\33"+
    "\2\34\1\1\1\35\4\1\1\36\1\37\14\3\1\40"+
    "\7\3\1\41\10\3\1\0\1\5\1\42\1\0\1\5"+
    "\1\0\1\42\1\0\1\3\3\0\1\43\1\44\1\45"+
    "\1\46\1\47\1\50\4\0\1\51\4\3\1\52\13\3"+
    "\1\53\1\3\1\54\3\3\1\55\11\3\1\56\1\57"+
    "\1\42\1\0\1\5\1\0\1\60\1\0\1\60\10\0"+
    "\4\3\1\61\1\3\1\62\1\63\2\3\1\64\2\3"+
    "\1\65\5\3\1\66\10\3\1\67\11\0\2\3\1\70"+
    "\1\71\1\72\2\3\1\73\1\74\1\3\1\75\1\76"+
    "\4\3\1\77\1\100\3\3\1\101\11\0\1\3\1\102"+
    "\1\103\3\3\1\104\2\3\1\105\3\3\1\0\2\3"+
    "\1\106\1\107\2\3\1\110\1\3\1\111\1\112\1\113"+
    "\1\114\1\115\1\3\1\116";

  private static int [] zzUnpackAction() {
    int [] result = new int[267];
    int offset = 0;
    offset = zzUnpackAction(ZZ_ACTION_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAction(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /** 
   * Translates a state to a row index in the transition table
   */
  private static final int [] ZZ_ROWMAP = zzUnpackRowMap();

  private static final String ZZ_ROWMAP_PACKED_0 =
    "\0\0\0\102\0\204\0\306\0\u0108\0\u014a\0\u018c\0\u01ce"+
    "\0\u0210\0\u0252\0\u0294\0\u02d6\0\u0318\0\u035a\0\u039c\0\u03de"+
    "\0\u0420\0\u0462\0\u04a4\0\u04e6\0\u0528\0\u056a\0\u05ac\0\u014a"+
    "\0\u05ee\0\u0630\0\u0672\0\u014a\0\u014a\0\u06b4\0\u06f6\0\u01ce"+
    "\0\u014a\0\u0738\0\u014a\0\u077a\0\u014a\0\u07bc\0\u07fe\0\u014a"+
    "\0\u014a\0\u014a\0\u014a\0\u014a\0\u014a\0\u014a\0\u0840\0\u014a"+
    "\0\u014a\0\u014a\0\u014a\0\u014a\0\u014a\0\u014a\0\u0882\0\u08c4"+
    "\0\u0906\0\u0948\0\u098a\0\u014a\0\u09cc\0\u014a\0\u014a\0\u0a0e"+
    "\0\u0a50\0\u0a92\0\u014a\0\u014a\0\u0ad4\0\u0b16\0\u0b58\0\u0b9a"+
    "\0\u0bdc\0\u0c1e\0\u0c60\0\u0ca2\0\u0ce4\0\u0d26\0\u0d68\0\u0daa"+
    "\0\u01ce\0\u0dec\0\u0e2e\0\u0e70\0\u0eb2\0\u0ef4\0\u0f36\0\u0f78"+
    "\0\u01ce\0\u0fba\0\u0ffc\0\u103e\0\u1080\0\u10c2\0\u1104\0\u1146"+
    "\0\u1188\0\u11ca\0\u014a\0\u014a\0\u120c\0\u124e\0\u1290\0\u12d2"+
    "\0\u1314\0\u1356\0\u1398\0\u13da\0\u141c\0\u014a\0\u014a\0\u014a"+
    "\0\u014a\0\u014a\0\u014a\0\u145e\0\u14a0\0\u14e2\0\u0a92\0\u0a92"+
    "\0\u1524\0\u1566\0\u15a8\0\u15ea\0\u01ce\0\u162c\0\u166e\0\u16b0"+
    "\0\u16f2\0\u1734\0\u1776\0\u17b8\0\u17fa\0\u183c\0\u187e\0\u18c0"+
    "\0\u01ce\0\u1902\0\u01ce\0\u1944\0\u1986\0\u19c8\0\u01ce\0\u1a0a"+
    "\0\u1a4c\0\u1a8e\0\u1ad0\0\u1b12\0\u1b54\0\u1b96\0\u1bd8\0\u1c1a"+
    "\0\u01ce\0\u01ce\0\u1c5c\0\u1c9e\0\u1ce0\0\u1356\0\u1356\0\u1d22"+
    "\0\u014a\0\u1d64\0\u1da6\0\u1de8\0\u1e2a\0\u1e6c\0\u1eae\0\u1ef0"+
    "\0\u1f32\0\u1f74\0\u1fb6\0\u1ff8\0\u203a\0\u01ce\0\u207c\0\u01ce"+
    "\0\u01ce\0\u20be\0\u2100\0\u01ce\0\u2142\0\u2184\0\u01ce\0\u21c6"+
    "\0\u2208\0\u224a\0\u228c\0\u22ce\0\u01ce\0\u2310\0\u2352\0\u2394"+
    "\0\u23d6\0\u2418\0\u245a\0\u249c\0\u24de\0\u01ce\0\u2520\0\u2562"+
    "\0\u25a4\0\u25e6\0\u2628\0\u266a\0\u26ac\0\u26ee\0\u2730\0\u2772"+
    "\0\u27b4\0\u01ce\0\u01ce\0\u01ce\0\u27f6\0\u2838\0\u01ce\0\u01ce"+
    "\0\u287a\0\u01ce\0\u28bc\0\u28fe\0\u2940\0\u2982\0\u29c4\0\u01ce"+
    "\0\u01ce\0\u2a06\0\u2a48\0\u2a8a\0\u01ce\0\u2acc\0\u2b0e\0\u2b50"+
    "\0\u2b92\0\u2bd4\0\u2c16\0\u2c58\0\u2c9a\0\u2cdc\0\u2d1e\0\u01ce"+
    "\0\u01ce\0\u2d60\0\u2da2\0\u2de4\0\u01ce\0\u2e26\0\u2e68\0\u01ce"+
    "\0\u2eaa\0\u2eec\0\u2f2e\0\u2f70\0\u2fb2\0\u2ff4\0\u01ce\0\u01ce"+
    "\0\u3036\0\u3078\0\u01ce\0\u30ba\0\u01ce\0\u01ce\0\u01ce\0\u01ce"+
    "\0\u01ce\0\u30fc\0\u01ce";

  private static int [] zzUnpackRowMap() {
    int [] result = new int[267];
    int offset = 0;
    offset = zzUnpackRowMap(ZZ_ROWMAP_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackRowMap(String packed, int offset, int [] result) {
    int i = 0;  /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int high = packed.charAt(i++) << 16;
      result[j++] = high | packed.charAt(i++);
    }
    return j;
  }

  /** 
   * The transition table of the DFA
   */
  private static final int [] ZZ_TRANS = zzUnpackTrans();

  private static final String ZZ_TRANS_PACKED_0 =
    "\1\6\1\7\1\10\1\11\1\12\1\13\1\14\1\15"+
    "\2\10\1\16\1\17\1\20\1\10\1\21\1\22\1\23"+
    "\1\24\1\25\1\26\2\10\1\27\3\10\1\30\1\10"+
    "\1\31\2\32\1\10\1\33\1\10\1\34\1\35\1\10"+
    "\1\36\1\37\1\10\3\32\1\40\1\41\1\42\1\43"+
    "\1\6\1\44\1\45\1\46\1\47\1\50\1\51\1\52"+
    "\1\53\1\54\1\55\1\56\1\57\1\60\1\61\1\62"+
    "\1\63\1\64\1\65\46\66\1\67\6\66\1\70\3\66"+
    "\1\71\66\66\1\72\6\66\1\73\1\74\23\66\54\75"+
    "\1\76\25\75\46\77\1\100\6\77\1\101\4\77\1\102"+
    "\3\77\1\103\1\104\12\77\103\0\1\10\1\105\27\10"+
    "\1\0\5\10\1\0\1\10\2\0\1\10\2\0\5\10"+
    "\27\0\31\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\6\10\1\106\14\10\1\107\5\10"+
    "\1\0\5\10\1\0\1\10\2\0\1\10\2\0\5\10"+
    "\27\0\4\10\1\110\2\10\1\111\7\10\1\112\11\10"+
    "\1\0\5\10\1\0\1\10\2\0\1\10\2\0\5\10"+
    "\27\0\6\10\1\113\22\10\1\0\5\10\1\0\1\10"+
    "\2\0\1\10\2\0\5\10\27\0\1\114\7\10\1\115"+
    "\20\10\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\27\0\10\10\1\116\3\10\1\117\14\10\1\0"+
    "\5\10\1\0\1\10\2\0\1\10\2\0\5\10\27\0"+
    "\6\10\1\120\4\10\1\121\15\10\1\0\5\10\1\0"+
    "\1\10\2\0\1\10\2\0\5\10\27\0\1\122\12\10"+
    "\1\123\2\10\1\124\12\10\1\0\5\10\1\0\1\10"+
    "\2\0\1\10\2\0\5\10\27\0\1\10\1\125\23\10"+
    "\1\126\3\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\6\10\1\127\14\10\1\130\5\10"+
    "\1\0\5\10\1\0\1\10\2\0\1\10\2\0\5\10"+
    "\27\0\12\10\1\131\5\10\1\132\10\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\16\10"+
    "\1\133\12\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\1\134\30\10\1\0\5\10\1\0"+
    "\1\10\2\0\1\10\2\0\5\10\27\0\1\135\3\10"+
    "\1\136\24\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\7\10\1\137\6\10\1\140\12\10"+
    "\1\0\5\10\1\0\1\10\2\0\1\10\2\0\5\10"+
    "\27\0\1\141\30\10\1\0\5\10\1\0\1\10\2\0"+
    "\1\10\2\0\5\10\35\0\1\142\1\0\1\143\2\144"+
    "\1\0\1\145\15\0\1\143\1\146\1\147\1\146\1\0"+
    "\1\150\1\142\2\0\1\144\3\0\2\146\1\147\36\0"+
    "\1\142\1\0\1\143\2\144\17\0\1\143\3\32\1\0"+
    "\1\150\1\142\2\0\1\144\3\0\3\32\63\0\3\150"+
    "\11\0\3\150\27\0\45\151\1\152\1\153\5\151\1\154"+
    "\25\151\24\0\1\155\32\0\1\43\100\0\1\43\103\0"+
    "\1\156\1\157\63\0\1\160\26\0\1\161\101\0\1\162"+
    "\72\0\1\163\42\0\1\164\32\0\1\66\100\0\1\66"+
    "\103\0\1\74\45\0\1\165\32\0\1\74\100\0\1\74"+
    "\23\0\54\75\1\0\25\75\24\0\1\166\32\0\1\77"+
    "\100\0\1\77\23\0\54\167\1\0\6\167\1\170\16\167"+
    "\1\0\2\10\1\171\26\10\1\0\5\10\1\0\1\10"+
    "\2\0\1\10\2\0\5\10\27\0\1\172\30\10\1\0"+
    "\5\10\1\0\1\10\2\0\1\10\2\0\5\10\27\0"+
    "\21\10\1\173\7\10\1\0\5\10\1\0\1\10\2\0"+
    "\1\10\2\0\5\10\27\0\1\174\16\10\1\175\3\10"+
    "\1\176\5\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\4\10\1\177\11\10\1\200\12\10"+
    "\1\0\5\10\1\0\1\10\2\0\1\10\2\0\5\10"+
    "\27\0\21\10\1\201\7\10\1\0\5\10\1\0\1\10"+
    "\2\0\1\10\2\0\5\10\27\0\3\10\1\202\24\10"+
    "\1\203\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\27\0\2\10\1\204\1\205\25\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\1\206"+
    "\30\10\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\27\0\2\10\1\207\26\10\1\0\5\10\1\0"+
    "\1\10\2\0\1\10\2\0\5\10\27\0\3\10\1\210"+
    "\25\10\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\27\0\12\10\1\211\16\10\1\0\5\10\1\0"+
    "\1\10\2\0\1\10\2\0\5\10\27\0\10\10\1\212"+
    "\20\10\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\27\0\4\10\1\213\24\10\1\0\5\10\1\0"+
    "\1\10\2\0\1\10\2\0\5\10\27\0\15\10\1\214"+
    "\13\10\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\27\0\24\10\1\215\4\10\1\0\5\10\1\0"+
    "\1\10\2\0\1\10\2\0\5\10\27\0\6\10\1\216"+
    "\22\10\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\27\0\22\10\1\217\6\10\1\0\5\10\1\0"+
    "\1\10\2\0\1\10\2\0\5\10\27\0\10\10\1\220"+
    "\20\10\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\27\0\21\10\1\221\7\10\1\0\5\10\1\0"+
    "\1\10\2\0\1\10\2\0\5\10\27\0\6\10\1\222"+
    "\22\10\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\27\0\3\10\1\223\25\10\1\0\5\10\1\0"+
    "\1\10\2\0\1\10\2\0\5\10\27\0\5\10\1\224"+
    "\23\10\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\27\0\13\10\1\225\2\10\1\226\12\10\1\0"+
    "\5\10\1\0\1\10\2\0\1\10\2\0\5\10\27\0"+
    "\16\10\1\227\12\10\1\0\5\10\1\0\1\10\2\0"+
    "\1\10\2\0\5\10\27\0\3\10\1\230\25\10\1\0"+
    "\5\10\1\0\1\10\2\0\1\10\2\0\5\10\27\0"+
    "\4\10\1\231\3\10\1\232\20\10\1\0\5\10\1\0"+
    "\1\10\2\0\1\10\2\0\5\10\62\0\3\233\3\0"+
    "\2\234\4\0\3\233\30\0\2\235\3\0\2\235\2\0"+
    "\2\235\20\0\4\235\1\0\1\235\2\0\1\235\3\0"+
    "\3\235\36\0\1\142\1\0\1\143\2\144\17\0\1\143"+
    "\1\146\1\147\1\146\1\0\1\150\1\142\2\0\1\144"+
    "\3\0\2\146\1\147\36\0\1\142\2\0\2\144\20\0"+
    "\3\147\1\0\1\150\1\142\2\0\1\144\3\0\3\147"+
    "\36\0\1\142\2\0\2\144\20\0\3\150\2\0\1\142"+
    "\2\0\1\144\3\0\3\150\27\0\45\236\1\237\6\236"+
    "\1\0\72\236\1\152\6\236\1\0\51\236\1\240\20\236"+
    "\1\237\6\236\1\0\25\236\45\0\1\241\70\0\1\242"+
    "\13\0\1\243\65\0\1\244\13\0\1\245\65\0\1\246"+
    "\13\0\1\247\65\0\1\250\13\0\1\251\32\0\3\10"+
    "\1\252\25\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\10\10\1\253\20\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\6\10"+
    "\1\254\22\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\16\10\1\255\12\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\6\10"+
    "\1\256\22\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\13\10\1\257\15\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\2\10"+
    "\1\260\26\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\6\10\1\261\22\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\23\10"+
    "\1\262\5\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\23\10\1\263\5\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\6\10"+
    "\1\264\22\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\5\10\1\265\23\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\2\10"+
    "\1\266\26\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\6\10\1\267\22\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\6\10"+
    "\1\270\22\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\2\10\1\271\26\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\1\272"+
    "\30\10\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\27\0\6\10\1\273\22\10\1\0\5\10\1\0"+
    "\1\10\2\0\1\10\2\0\5\10\27\0\4\10\1\274"+
    "\24\10\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\27\0\10\10\1\275\20\10\1\0\5\10\1\0"+
    "\1\10\2\0\1\10\2\0\5\10\27\0\10\10\1\276"+
    "\2\10\1\277\15\10\1\0\5\10\1\0\1\10\2\0"+
    "\1\10\2\0\5\10\27\0\10\10\1\300\20\10\1\0"+
    "\5\10\1\0\1\10\2\0\1\10\2\0\5\10\27\0"+
    "\5\10\1\301\23\10\1\0\5\10\1\0\1\10\2\0"+
    "\1\10\2\0\5\10\27\0\26\10\1\302\2\10\1\0"+
    "\5\10\1\0\1\10\2\0\1\10\2\0\5\10\27\0"+
    "\3\10\1\303\25\10\1\0\5\10\1\0\1\10\2\0"+
    "\1\10\2\0\5\10\27\0\25\10\1\304\3\10\1\0"+
    "\5\10\1\0\1\10\2\0\1\10\2\0\5\10\27\0"+
    "\10\10\1\305\20\10\1\0\5\10\1\0\1\10\2\0"+
    "\1\10\2\0\5\10\27\0\7\10\1\306\21\10\1\0"+
    "\5\10\1\0\1\10\2\0\1\10\2\0\5\10\40\0"+
    "\2\144\20\0\3\233\5\0\1\144\3\0\3\233\63\0"+
    "\3\233\11\0\3\233\30\0\2\235\3\0\2\235\1\0"+
    "\1\143\2\235\17\0\1\143\4\235\1\0\1\235\2\0"+
    "\1\235\3\0\3\235\27\0\1\236\2\307\3\236\2\307"+
    "\2\236\2\307\20\236\4\307\1\236\1\307\2\236\1\307"+
    "\1\152\2\236\3\307\1\236\1\0\25\236\34\0\1\310"+
    "\101\0\1\311\101\0\1\312\101\0\1\313\101\0\1\314"+
    "\101\0\1\315\101\0\1\316\101\0\1\317\46\0\4\10"+
    "\1\320\24\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\6\10\1\321\22\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\4\10"+
    "\1\322\24\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\3\10\1\323\25\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\22\10"+
    "\1\324\6\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\4\10\1\325\24\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\16\10"+
    "\1\326\12\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\7\10\1\327\21\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\2\10"+
    "\1\330\26\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\15\10\1\331\13\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\6\10"+
    "\1\332\22\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\10\10\1\333\20\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\5\10"+
    "\1\334\23\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\4\10\1\335\24\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\16\10"+
    "\1\336\12\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\4\10\1\337\24\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\11\10"+
    "\1\340\17\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\7\10\1\341\21\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\1\342"+
    "\30\10\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\27\0\6\10\1\343\22\10\1\0\5\10\1\0"+
    "\1\10\2\0\1\10\2\0\5\10\27\0\1\344\30\10"+
    "\1\0\5\10\1\0\1\10\2\0\1\10\2\0\5\10"+
    "\27\0\6\10\1\345\22\10\1\0\5\10\1\0\1\10"+
    "\2\0\1\10\2\0\5\10\26\0\1\236\2\346\3\236"+
    "\2\346\2\236\2\346\20\236\4\346\1\236\1\346\2\236"+
    "\1\346\1\152\2\236\3\346\1\236\1\0\25\236\52\0"+
    "\1\347\77\0\1\350\103\0\1\351\77\0\1\352\103\0"+
    "\1\353\77\0\1\354\103\0\1\355\77\0\1\356\32\0"+
    "\1\357\30\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\11\10\1\360\17\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\15\10"+
    "\1\361\13\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\4\10\1\362\24\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\11\10"+
    "\1\363\17\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\10\10\1\364\20\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\3\10"+
    "\1\365\25\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\16\10\1\366\12\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\5\10"+
    "\1\367\23\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\3\10\1\370\25\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\27\10"+
    "\1\371\1\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\27\0\5\10\1\372\23\10\1\0\5\10"+
    "\1\0\1\10\2\0\1\10\2\0\5\10\27\0\3\10"+
    "\1\373\25\10\1\0\5\10\1\0\1\10\2\0\1\10"+
    "\2\0\5\10\26\0\1\236\2\374\3\236\2\374\2\236"+
    "\2\374\20\236\4\374\1\236\1\374\2\236\1\374\1\152"+
    "\2\236\3\374\1\236\1\0\25\236\51\0\1\43\102\0"+
    "\1\43\100\0\1\66\102\0\1\66\100\0\1\74\102\0"+
    "\1\74\100\0\1\77\102\0\1\77\30\0\5\10\1\375"+
    "\23\10\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\27\0\6\10\1\376\22\10\1\0\5\10\1\0"+
    "\1\10\2\0\1\10\2\0\5\10\27\0\2\10\1\377"+
    "\26\10\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\27\0\17\10\1\u0100\11\10\1\0\5\10\1\0"+
    "\1\10\2\0\1\10\2\0\5\10\27\0\11\10\1\u0101"+
    "\17\10\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\27\0\16\10\1\u0102\12\10\1\0\5\10\1\0"+
    "\1\10\2\0\1\10\2\0\5\10\27\0\6\10\1\u0103"+
    "\22\10\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\27\0\3\10\1\u0104\25\10\1\0\5\10\1\0"+
    "\1\10\2\0\1\10\2\0\5\10\27\0\6\10\1\u0105"+
    "\22\10\1\0\5\10\1\0\1\10\2\0\1\10\2\0"+
    "\5\10\26\0\1\236\2\151\3\236\2\151\2\236\2\151"+
    "\20\236\4\151\1\236\1\151\2\236\1\151\1\152\2\236"+
    "\3\151\1\236\1\0\25\236\1\0\3\10\1\u0106\25\10"+
    "\1\0\5\10\1\0\1\10\2\0\1\10\2\0\5\10"+
    "\27\0\2\10\1\u0107\26\10\1\0\5\10\1\0\1\10"+
    "\2\0\1\10\2\0\5\10\27\0\6\10\1\u0108\22\10"+
    "\1\0\5\10\1\0\1\10\2\0\1\10\2\0\5\10"+
    "\27\0\3\10\1\u0109\25\10\1\0\5\10\1\0\1\10"+
    "\2\0\1\10\2\0\5\10\27\0\6\10\1\u010a\22\10"+
    "\1\0\5\10\1\0\1\10\2\0\1\10\2\0\5\10"+
    "\27\0\11\10\1\u010b\17\10\1\0\5\10\1\0\1\10"+
    "\2\0\1\10\2\0\5\10\26\0";

  private static int [] zzUnpackTrans() {
    int [] result = new int[12606];
    int offset = 0;
    offset = zzUnpackTrans(ZZ_TRANS_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackTrans(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      value--;
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /* error codes */
  private static final int ZZ_UNKNOWN_ERROR = 0;
  private static final int ZZ_NO_MATCH = 1;
  private static final int ZZ_PUSHBACK_2BIG = 2;
  private static final char[] EMPTY_BUFFER = new char[0];
  private static final int YYEOF = -1;
  private static java.io.Reader zzReader = null; // Fake

  /* error messages for the codes above */
  private static final String ZZ_ERROR_MSG[] = {
    "Unkown internal scanner error",
    "Error: could not match input",
    "Error: pushback value was too large"
  };

  /**
   * ZZ_ATTRIBUTE[aState] contains the attributes of state <code>aState</code>
   */
  private static final int [] ZZ_ATTRIBUTE = zzUnpackAttribute();

  private static final String ZZ_ATTRIBUTE_PACKED_0 =
    "\3\0\1\1\1\0\1\11\21\1\1\11\3\1\2\11"+
    "\3\1\1\11\1\1\1\11\1\1\1\11\2\1\7\11"+
    "\1\1\7\11\5\1\1\11\1\1\2\11\3\1\2\11"+
    "\35\1\1\0\2\11\1\0\1\1\1\0\1\1\1\0"+
    "\1\1\3\0\6\11\4\0\44\1\1\0\1\1\1\0"+
    "\1\1\1\0\1\11\10\0\35\1\11\0\26\1\11\0"+
    "\15\1\1\0\17\1";

  private static int [] zzUnpackAttribute() {
    int [] result = new int[267];
    int offset = 0;
    offset = zzUnpackAttribute(ZZ_ATTRIBUTE_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAttribute(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }

  /** the current state of the DFA */
  private int zzState;

  /** the current lexical state */
  private int zzLexicalState = YYINITIAL;

  /** this buffer contains the current text to be matched and is
      the source of the yytext() string */
  private CharSequence zzBuffer = "";

  /** the textposition at the last accepting state */
  private int zzMarkedPos;

  /** the textposition at the last state to be included in yytext */
  private int zzPushbackPos;

  /** the current text position in the buffer */
  private int zzCurrentPos;

  /** startRead marks the beginning of the yytext() string in the buffer */
  private int zzStartRead;

  /** endRead marks the last character in the buffer, that has been read
      from input */
  private int zzEndRead;

  /**
   * zzAtBOL == true <=> the scanner is currently at the beginning of a line
   */
  private boolean zzAtBOL = true;

  /** zzAtEOF == true <=> the scanner is at the EOF */
  private boolean zzAtEOF;

  /** denotes if the user-EOF-code has already been executed */
  private boolean zzEOFDone;

  /* user code: */
    private IElementType process(IElementType type){
        //System.out.println(type.toString());
        return type;
    }



  public _ScalaLexer(java.io.Reader in) {
    this.zzReader = in;
  }

  /**
   * Creates a new scanner.
   * There is also java.io.Reader version of this constructor.
   *
   * @param   in  the java.io.Inputstream to read input from.
   */
  public _ScalaLexer(java.io.InputStream in) {
    this(new java.io.InputStreamReader(in));
  }

  /** 
   * Unpacks the compressed character translation table.
   *
   * @param packed   the packed character translation table
   * @return         the unpacked character translation table
   */
  private static char [] zzUnpackCMap(String packed) {
    char [] map = new char[0x10000];
    int i = 0;  /* index in packed string  */
    int j = 0;  /* index in unpacked array */
    while (i < 172) {
      int  count = packed.charAt(i++);
      char value = packed.charAt(i++);
      do map[j++] = value; while (--count > 0);
    }
    return map;
  }

  public final int getTokenStart(){
    return zzStartRead;
  }

  public final int getTokenEnd(){
    return getTokenStart() + yylength();
  }

  public void reset(CharSequence buffer, int initialState){
    zzBuffer = buffer;
    zzCurrentPos = zzMarkedPos = zzStartRead = 0;
    zzPushbackPos = 0;
    zzAtEOF  = false;
    zzAtBOL = true;
    zzEndRead = buffer.length();
    yybegin(initialState);
  }

  /**
   * Refills the input buffer.
   *
   * @return      <code>false</code>, iff there was new input.
   *
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  private boolean zzRefill() throws java.io.IOException {
    return true;
  }


  /**
   * Returns the current lexical state.
   */
  public final int yystate() {
    return zzLexicalState;
  }


  /**
   * Enters a new lexical state
   *
   * @param newState the new lexical state
   */
  public final void yybegin(int newState) {
    zzLexicalState = newState;
  }


  /**
   * Returns the text matched by the current regular expression.
   */
  public final CharSequence yytext() {
    return zzBuffer.subSequence(zzStartRead, zzMarkedPos);
  }


  /**
   * Returns the character at position <tt>pos</tt> from the
   * matched text.
   *
   * It is equivalent to yytext().charAt(pos), but faster
   *
   * @param pos the position of the character to fetch.
   *            A value from 0 to yylength()-1.
   *
   * @return the character at position pos
   */
  public final char yycharat(int pos) {
    return zzBuffer.charAt(zzStartRead+pos);
  }


  /**
   * Returns the length of the matched text region.
   */
  public final int yylength() {
    return zzMarkedPos-zzStartRead;
  }


  /**
   * Reports an error that occured while scanning.
   *
   * In a wellformed scanner (no or only correct usage of
   * yypushback(int) and a match-all fallback rule) this method
   * will only be called with things that "Can't Possibly Happen".
   * If this method is called, something is seriously wrong
   * (e.g. a JFlex bug producing a faulty scanner etc.).
   *
   * Usual syntax/scanner level error handling should be done
   * in error fallback rules.
   *
   * @param   errorCode  the code of the errormessage to display
   */
  private void zzScanError(int errorCode) {
    String message;
    try {
      message = ZZ_ERROR_MSG[errorCode];
    }
    catch (ArrayIndexOutOfBoundsException e) {
      message = ZZ_ERROR_MSG[ZZ_UNKNOWN_ERROR];
    }

    throw new Error(message);
  }


  /**
   * Pushes the specified amount of characters back into the input stream.
   *
   * They will be read again by then next call of the scanning method
   *
   * @param number  the number of characters to be read again.
   *                This number must not be greater than yylength()!
   */
  public void yypushback(int number)  {
    if ( number > yylength() )
      zzScanError(ZZ_PUSHBACK_2BIG);

    zzMarkedPos -= number;
  }


  /**
   * Contains user EOF-code, which will be executed exactly once,
   * when the end of file is reached
   */
  private void zzDoEOF() {
    if (!zzEOFDone) {
      zzEOFDone = true;
    
    }
  }


  /**
   * Resumes scanning until the next regular expression is matched,
   * the end of input is encountered or an I/O-Error occurs.
   *
   * @return      the next token
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  public IElementType advance() throws java.io.IOException {
    int zzInput;
    int zzAction;

    // cached fields:
    int zzCurrentPosL;
    int zzMarkedPosL;
    int zzEndReadL = zzEndRead;
    CharSequence zzBufferL = zzBuffer;
    char [] zzCMapL = ZZ_CMAP;

    int [] zzTransL = ZZ_TRANS;
    int [] zzRowMapL = ZZ_ROWMAP;
    int [] zzAttrL = ZZ_ATTRIBUTE;

    while (true) {
      zzMarkedPosL = zzMarkedPos;

      zzAction = -1;

      zzCurrentPosL = zzCurrentPos = zzStartRead = zzMarkedPosL;

      zzState = zzLexicalState;


      zzForAction: {
        while (true) {

          if (zzCurrentPosL < zzEndReadL)
            zzInput = zzBufferL.charAt(zzCurrentPosL++);
          else if (zzAtEOF) {
            zzInput = YYEOF;
            break zzForAction;
          }
          else {
            // store back cached positions
            zzCurrentPos  = zzCurrentPosL;
            zzMarkedPos   = zzMarkedPosL;
            boolean eof = zzRefill();
            // get translated positions and possibly new buffer
            zzCurrentPosL  = zzCurrentPos;
            zzMarkedPosL   = zzMarkedPos;
            zzBufferL      = zzBuffer;
            zzEndReadL     = zzEndRead;
            if (eof) {
              zzInput = YYEOF;
              break zzForAction;
            }
            else {
              zzInput = zzBufferL.charAt(zzCurrentPosL++);
            }
          }
          int zzNext = zzTransL[ zzRowMapL[zzState] + zzCMapL[zzInput] ];
          if (zzNext == -1) break zzForAction;
          zzState = zzNext;

          int zzAttributes = zzAttrL[zzState];
          if ( (zzAttributes & 1) == 1 ) {
            zzAction = zzState;
            zzMarkedPosL = zzCurrentPosL;
            if ( (zzAttributes & 8) == 8 ) break zzForAction;
          }

        }
      }

      // store back cached position
      zzMarkedPos = zzMarkedPosL;

      switch (zzAction < 0 ? zzAction : ZZ_ACTION[zzAction]) {
        case 59: 
          { return process(kCATCH);
          }
        case 79: break;
        case 57: 
          { return process(kTRAIT);
          }
        case 80: break;
        case 78: 
          { return process(kPROTECTED);
          }
        case 81: break;
        case 38: 
          { return process(tLOWER_BOUND);
          }
        case 82: break;
        case 29: 
          { yybegin(YYINITIAL);
                                            return process(tSTRING_END);
          }
        case 83: break;
        case 19: 
          { return process(tCOLON);
          }
        case 84: break;
        case 70: 
          { return process(kEXTENDS);
          }
        case 85: break;
        case 55: 
          { return process(kWHITH);
          }
        case 86: break;
        case 27: 
          { return process(tCOMMENT);
          }
        case 87: break;
        case 46: 
          { return process(kVAR);
          }
        case 88: break;
        case 67: 
          { return process(kRETURN);
          }
        case 89: break;
        case 25: 
          { return process(tSEMICOLON);
          }
        case 90: break;
        case 10: 
          { yybegin(IN_STRING_STATE);
                                            return process(tSTRING_BEGIN);
          }
        case 91: break;
        case 49: 
          { return process(kTRUE);
          }
        case 92: break;
        case 12: 
          { return process(tSTAR);
          }
        case 93: break;
        case 68: 
          { return process(kOBJECT);
          }
        case 94: break;
        case 2: 
          { return process(tSTUB);
          }
        case 95: break;
        case 39: 
          { return process(tUPPER_BOUND);
          }
        case 96: break;
        case 6: 
          { return process(tDOT);
          }
        case 97: break;
        case 23: 
          { return process(tTILDA);
          }
        case 98: break;
        case 64: 
          { return process(kMATCH);
          }
        case 99: break;
        case 26: 
          { return process(tCOMMA);
          }
        case 100: break;
        case 1: 
          { return process(tSTRING);
          }
        case 101: break;
        case 16: 
          { return process(tRBRACE);
          }
        case 102: break;
        case 50: 
          { return process(kTHIS);
          }
        case 103: break;
        case 58: 
          { return process(kTHROW);
          }
        case 104: break;
        case 47: 
          { return process(kVAL);
          }
        case 105: break;
        case 73: 
          { return process(kPRIVATE);
          }
        case 106: break;
        case 54: 
          { return process(kNULL);
          }
        case 107: break;
        case 24: 
          { return process(tNOT);
          }
        case 108: break;
        case 71: 
          { return process(kFINALLY);
          }
        case 109: break;
        case 17: 
          { return process(tLPARENTHIS);
          }
        case 110: break;
        case 4: 
          { return process(tWHITE_SPACE_IN_LINE);
          }
        case 111: break;
        case 66: 
          { return process(kSEALED);
          }
        case 112: break;
        case 31: 
          { yybegin(IN_XML_STATE);
                                            return process(tENDSCALAEXPR);
          }
        case 113: break;
        case 5: 
          { return process(tINTEGER);
          }
        case 114: break;
        case 60: 
          { return process(kCLASS);
          }
        case 115: break;
        case 76: 
          { return process(kOVERRIDE);
          }
        case 116: break;
        case 11: 
          { return process(tLINE_TERMINATOR);
          }
        case 117: break;
        case 72: 
          { return process(kPACKAGE);
          }
        case 118: break;
        case 13: 
          { return process(tLSQBRACKET);
          }
        case 119: break;
        case 15: 
          { return process(tLBRACE);
          }
        case 120: break;
        case 32: 
          { return process(kDO);
          }
        case 121: break;
        case 44: 
          { return process(kFOR);
          }
        case 122: break;
        case 61: 
          { return process(kFALSE);
          }
        case 123: break;
        case 37: 
          { return process(tCHOOSE);
          }
        case 124: break;
        case 63: 
          { return process(kYIELD);
          }
        case 125: break;
        case 22: 
          { return process(tAT);
          }
        case 126: break;
        case 3: 
          { return process(tIDENTIFIER);
          }
        case 127: break;
        case 45: 
          { return process(kNEW);
          }
        case 128: break;
        case 75: 
          { return process(kREQUIRES);
          }
        case 129: break;
        case 35: 
          { yybegin(IN_LINE_COMMENT_STATE);
                                            return process(tCOMMENT);
          }
        case 130: break;
        case 77: 
          { return process(kIMPLICIT);
          }
        case 131: break;
        case 7: 
          { return process(tPLUS);
          }
        case 132: break;
        case 62: 
          { return process(kFINAL);
          }
        case 133: break;
        case 42: 
          { return process(kTRY);
          }
        case 134: break;
        case 51: 
          { return process(kTYPE);
          }
        case 135: break;
        case 21: 
          { return process(tINNER_CLASS);
          }
        case 136: break;
        case 56: 
          { return process(kSUPER);
          }
        case 137: break;
        case 52: 
          { return process(kCASE);
          }
        case 138: break;
        case 9: 
          { return process(tUNDER);
          }
        case 139: break;
        case 20: 
          { return process(tASSIGN);
          }
        case 140: break;
        case 18: 
          { return process(tRPARENTHIS);
          }
        case 141: break;
        case 41: 
          { yybegin(IN_XML_STATE);
                                            return process(tOPENXMLTAG);
          }
        case 142: break;
        case 69: 
          { return process(kIMPORT);
          }
        case 143: break;
        case 74: 
          { return process(kABSTRACT);
          }
        case 144: break;
        case 34: 
          { return process(tFLOAT);
          }
        case 145: break;
        case 8: 
          { return process(tMINUS);
          }
        case 146: break;
        case 33: 
          { return process(kIF);
          }
        case 147: break;
        case 36: 
          { yybegin(IN_BLOCK_COMMENT_STATE);
                                            return process(tCOMMENT);
          }
        case 148: break;
        case 48: 
          { return process(tCHAR);
          }
        case 149: break;
        case 40: 
          { return process(tFUNTYPE);
          }
        case 150: break;
        case 14: 
          { return process(tRSQBRACKET);
          }
        case 151: break;
        case 30: 
          { yybegin(YYINITIAL);
                                            return process(tBEGINSCALAEXPR);
          }
        case 152: break;
        case 65: 
          { return process(kWHILE);
          }
        case 153: break;
        case 43: 
          { return process(kDEF);
          }
        case 154: break;
        case 53: 
          { return process(kELSE);
          }
        case 155: break;
        case 28: 
          { yybegin(YYINITIAL);
                                            return process(tCOMMENT);
          }
        case 156: break;
        default:
          if (zzInput == YYEOF && zzStartRead == zzCurrentPos) {
            zzAtEOF = true;
            zzDoEOF();
            return null;
          }
          else {
            zzScanError(ZZ_NO_MATCH);
          }
      }
    }
  }


}
