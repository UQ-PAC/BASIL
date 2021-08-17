// Generated from Bil.g4 by ANTLR 4.9.2
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class BilLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.9.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, T__10=11, T__11=12, T__12=13, T__13=14, T__14=15, T__15=16, T__16=17, 
		T__17=18, T__18=19, T__19=20, T__20=21, T__21=22, T__22=23, CAST=24, NAT=25, 
		ENDIAN=26, ID=27, NUMBER=28, DECIMAL=29, HEX=30, ALPHA=31, NEWLINE=32, 
		WHITESPACE=33, COMMENT=34, PLUS=35, MINUS=36, TIMES=37, DIVIDE=38, MODULO=39, 
		LSL=40, LSR=41, ASR=42, BAND=43, BOR=44, BXOR=45, EQ=46, NEQ=47, LT=48, 
		LE=49, NOT=50;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"T__0", "T__1", "T__2", "T__3", "T__4", "T__5", "T__6", "T__7", "T__8", 
			"T__9", "T__10", "T__11", "T__12", "T__13", "T__14", "T__15", "T__16", 
			"T__17", "T__18", "T__19", "T__20", "T__21", "T__22", "CAST", "NAT", 
			"ENDIAN", "ID", "NUMBER", "DECIMAL", "HEX", "ALPHA", "NEWLINE", "WHITESPACE", 
			"COMMENT", "PLUS", "MINUS", "TIMES", "DIVIDE", "MODULO", "LSL", "LSR", 
			"ASR", "BAND", "BOR", "BXOR", "EQ", "NEQ", "LT", "LE", "NOT"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "':'", "'program'", "'sub'", "'('", "','", "')'", "'::'", "'call'", 
			"'@'", "'with'", "'noreturn'", "':='", "'['", "']:'", "'<-'", "']'", 
			"'extract'", "'when'", "'goto'", "'in out'", "'in'", "'out'", "'return'", 
			null, null, null, null, null, null, null, null, null, null, null, "'+'", 
			"'-'", "'*'", "'/'", "'%'", "'<<'", "'>>'", "'>>>'", "'&'", "'|'", "'xor'", 
			"'='", "'<>'", "'<'", "'<='", "'~'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, null, null, null, null, null, null, null, null, 
			null, null, null, null, null, null, null, null, null, null, null, null, 
			"CAST", "NAT", "ENDIAN", "ID", "NUMBER", "DECIMAL", "HEX", "ALPHA", "NEWLINE", 
			"WHITESPACE", "COMMENT", "PLUS", "MINUS", "TIMES", "DIVIDE", "MODULO", 
			"LSL", "LSR", "ASR", "BAND", "BOR", "BXOR", "EQ", "NEQ", "LT", "LE", 
			"NOT"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}


	public BilLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "Bil.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\64\u014b\b\1\4\2"+
		"\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4"+
		"\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22"+
		"\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31"+
		"\t\31\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t"+
		" \4!\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\4(\t(\4)\t)\4*\t*\4+\t"+
		"+\4,\t,\4-\t-\4.\t.\4/\t/\4\60\t\60\4\61\t\61\4\62\t\62\4\63\t\63\3\2"+
		"\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\4\3\4\3\4\3\4\3\5\3\5\3\6\3\6\3"+
		"\7\3\7\3\b\3\b\3\b\3\t\3\t\3\t\3\t\3\t\3\n\3\n\3\13\3\13\3\13\3\13\3\13"+
		"\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\r\3\r\3\r\3\16\3\16\3\17\3\17\3"+
		"\17\3\20\3\20\3\20\3\21\3\21\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3"+
		"\23\3\23\3\23\3\23\3\23\3\24\3\24\3\24\3\24\3\24\3\25\3\25\3\25\3\25\3"+
		"\25\3\25\3\25\3\26\3\26\3\26\3\27\3\27\3\27\3\27\3\30\3\30\3\30\3\30\3"+
		"\30\3\30\3\30\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3"+
		"\31\3\31\3\31\3\31\3\31\5\31\u00d8\n\31\3\32\3\32\3\32\3\32\3\32\3\32"+
		"\5\32\u00e0\n\32\3\33\3\33\3\33\3\33\5\33\u00e6\n\33\3\34\3\34\5\34\u00ea"+
		"\n\34\3\34\3\34\3\34\7\34\u00ef\n\34\f\34\16\34\u00f2\13\34\3\35\3\35"+
		"\5\35\u00f6\n\35\3\36\6\36\u00f9\n\36\r\36\16\36\u00fa\3\37\3\37\5\37"+
		"\u00ff\n\37\3\37\6\37\u0102\n\37\r\37\16\37\u0103\3 \6 \u0107\n \r \16"+
		" \u0108\3!\5!\u010c\n!\3!\3!\3!\3!\3\"\6\"\u0113\n\"\r\"\16\"\u0114\3"+
		"\"\3\"\3#\3#\3#\3#\7#\u011d\n#\f#\16#\u0120\13#\3#\3#\3$\3$\3%\3%\3&\3"+
		"&\3\'\3\'\3(\3(\3)\3)\3)\3*\3*\3*\3+\3+\3+\3+\3,\3,\3-\3-\3.\3.\3.\3."+
		"\3/\3/\3\60\3\60\3\60\3\61\3\61\3\62\3\62\3\62\3\63\3\63\2\2\64\3\3\5"+
		"\4\7\5\t\6\13\7\r\b\17\t\21\n\23\13\25\f\27\r\31\16\33\17\35\20\37\21"+
		"!\22#\23%\24\'\25)\26+\27-\30/\31\61\32\63\33\65\34\67\359\36;\37= ?!"+
		"A\"C#E$G%I&K\'M(O)Q*S+U,W-Y.[/]\60_\61a\62c\63e\64\3\2\7\4\2%%aa\3\2\62"+
		";\5\2\62;CHch\4\2C\\c|\4\2\f\f\17\17\2\u015b\2\3\3\2\2\2\2\5\3\2\2\2\2"+
		"\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2"+
		"\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2"+
		"\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2\2\2\2%\3\2\2\2\2\'\3\2\2"+
		"\2\2)\3\2\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2\2\2\61\3\2\2\2\2\63\3\2\2"+
		"\2\2\65\3\2\2\2\2\67\3\2\2\2\29\3\2\2\2\2;\3\2\2\2\2=\3\2\2\2\2?\3\2\2"+
		"\2\2A\3\2\2\2\2C\3\2\2\2\2E\3\2\2\2\2G\3\2\2\2\2I\3\2\2\2\2K\3\2\2\2\2"+
		"M\3\2\2\2\2O\3\2\2\2\2Q\3\2\2\2\2S\3\2\2\2\2U\3\2\2\2\2W\3\2\2\2\2Y\3"+
		"\2\2\2\2[\3\2\2\2\2]\3\2\2\2\2_\3\2\2\2\2a\3\2\2\2\2c\3\2\2\2\2e\3\2\2"+
		"\2\3g\3\2\2\2\5i\3\2\2\2\7q\3\2\2\2\tu\3\2\2\2\13w\3\2\2\2\ry\3\2\2\2"+
		"\17{\3\2\2\2\21~\3\2\2\2\23\u0083\3\2\2\2\25\u0085\3\2\2\2\27\u008a\3"+
		"\2\2\2\31\u0093\3\2\2\2\33\u0096\3\2\2\2\35\u0098\3\2\2\2\37\u009b\3\2"+
		"\2\2!\u009e\3\2\2\2#\u00a0\3\2\2\2%\u00a8\3\2\2\2\'\u00ad\3\2\2\2)\u00b2"+
		"\3\2\2\2+\u00b9\3\2\2\2-\u00bc\3\2\2\2/\u00c0\3\2\2\2\61\u00d7\3\2\2\2"+
		"\63\u00df\3\2\2\2\65\u00e5\3\2\2\2\67\u00e9\3\2\2\29\u00f5\3\2\2\2;\u00f8"+
		"\3\2\2\2=\u00fe\3\2\2\2?\u0106\3\2\2\2A\u010b\3\2\2\2C\u0112\3\2\2\2E"+
		"\u0118\3\2\2\2G\u0123\3\2\2\2I\u0125\3\2\2\2K\u0127\3\2\2\2M\u0129\3\2"+
		"\2\2O\u012b\3\2\2\2Q\u012d\3\2\2\2S\u0130\3\2\2\2U\u0133\3\2\2\2W\u0137"+
		"\3\2\2\2Y\u0139\3\2\2\2[\u013b\3\2\2\2]\u013f\3\2\2\2_\u0141\3\2\2\2a"+
		"\u0144\3\2\2\2c\u0146\3\2\2\2e\u0149\3\2\2\2gh\7<\2\2h\4\3\2\2\2ij\7r"+
		"\2\2jk\7t\2\2kl\7q\2\2lm\7i\2\2mn\7t\2\2no\7c\2\2op\7o\2\2p\6\3\2\2\2"+
		"qr\7u\2\2rs\7w\2\2st\7d\2\2t\b\3\2\2\2uv\7*\2\2v\n\3\2\2\2wx\7.\2\2x\f"+
		"\3\2\2\2yz\7+\2\2z\16\3\2\2\2{|\7<\2\2|}\7<\2\2}\20\3\2\2\2~\177\7e\2"+
		"\2\177\u0080\7c\2\2\u0080\u0081\7n\2\2\u0081\u0082\7n\2\2\u0082\22\3\2"+
		"\2\2\u0083\u0084\7B\2\2\u0084\24\3\2\2\2\u0085\u0086\7y\2\2\u0086\u0087"+
		"\7k\2\2\u0087\u0088\7v\2\2\u0088\u0089\7j\2\2\u0089\26\3\2\2\2\u008a\u008b"+
		"\7p\2\2\u008b\u008c\7q\2\2\u008c\u008d\7t\2\2\u008d\u008e\7g\2\2\u008e"+
		"\u008f\7v\2\2\u008f\u0090\7w\2\2\u0090\u0091\7t\2\2\u0091\u0092\7p\2\2"+
		"\u0092\30\3\2\2\2\u0093\u0094\7<\2\2\u0094\u0095\7?\2\2\u0095\32\3\2\2"+
		"\2\u0096\u0097\7]\2\2\u0097\34\3\2\2\2\u0098\u0099\7_\2\2\u0099\u009a"+
		"\7<\2\2\u009a\36\3\2\2\2\u009b\u009c\7>\2\2\u009c\u009d\7/\2\2\u009d "+
		"\3\2\2\2\u009e\u009f\7_\2\2\u009f\"\3\2\2\2\u00a0\u00a1\7g\2\2\u00a1\u00a2"+
		"\7z\2\2\u00a2\u00a3\7v\2\2\u00a3\u00a4\7t\2\2\u00a4\u00a5\7c\2\2\u00a5"+
		"\u00a6\7e\2\2\u00a6\u00a7\7v\2\2\u00a7$\3\2\2\2\u00a8\u00a9\7y\2\2\u00a9"+
		"\u00aa\7j\2\2\u00aa\u00ab\7g\2\2\u00ab\u00ac\7p\2\2\u00ac&\3\2\2\2\u00ad"+
		"\u00ae\7i\2\2\u00ae\u00af\7q\2\2\u00af\u00b0\7v\2\2\u00b0\u00b1\7q\2\2"+
		"\u00b1(\3\2\2\2\u00b2\u00b3\7k\2\2\u00b3\u00b4\7p\2\2\u00b4\u00b5\7\""+
		"\2\2\u00b5\u00b6\7q\2\2\u00b6\u00b7\7w\2\2\u00b7\u00b8\7v\2\2\u00b8*\3"+
		"\2\2\2\u00b9\u00ba\7k\2\2\u00ba\u00bb\7p\2\2\u00bb,\3\2\2\2\u00bc\u00bd"+
		"\7q\2\2\u00bd\u00be\7w\2\2\u00be\u00bf\7v\2\2\u00bf.\3\2\2\2\u00c0\u00c1"+
		"\7t\2\2\u00c1\u00c2\7g\2\2\u00c2\u00c3\7v\2\2\u00c3\u00c4\7w\2\2\u00c4"+
		"\u00c5\7t\2\2\u00c5\u00c6\7p\2\2\u00c6\60\3\2\2\2\u00c7\u00c8\7r\2\2\u00c8"+
		"\u00c9\7c\2\2\u00c9\u00d8\7f\2\2\u00ca\u00cb\7g\2\2\u00cb\u00cc\7z\2\2"+
		"\u00cc\u00cd\7v\2\2\u00cd\u00ce\7g\2\2\u00ce\u00cf\7p\2\2\u00cf\u00d8"+
		"\7f\2\2\u00d0\u00d1\7j\2\2\u00d1\u00d2\7k\2\2\u00d2\u00d3\7i\2\2\u00d3"+
		"\u00d8\7j\2\2\u00d4\u00d5\7n\2\2\u00d5\u00d6\7q\2\2\u00d6\u00d8\7y\2\2"+
		"\u00d7\u00c7\3\2\2\2\u00d7\u00ca\3\2\2\2\u00d7\u00d0\3\2\2\2\u00d7\u00d4"+
		"\3\2\2\2\u00d8\62\3\2\2\2\u00d9\u00da\7w\2\2\u00da\u00db\7\65\2\2\u00db"+
		"\u00e0\7\64\2\2\u00dc\u00dd\7w\2\2\u00dd\u00de\78\2\2\u00de\u00e0\7\66"+
		"\2\2\u00df\u00d9\3\2\2\2\u00df\u00dc\3\2\2\2\u00e0\64\3\2\2\2\u00e1\u00e2"+
		"\7g\2\2\u00e2\u00e6\7n\2\2\u00e3\u00e4\7d\2\2\u00e4\u00e6\7g\2\2\u00e5"+
		"\u00e1\3\2\2\2\u00e5\u00e3\3\2\2\2\u00e6\66\3\2\2\2\u00e7\u00ea\5? \2"+
		"\u00e8\u00ea\t\2\2\2\u00e9\u00e7\3\2\2\2\u00e9\u00e8\3\2\2\2\u00ea\u00f0"+
		"\3\2\2\2\u00eb\u00ef\5? \2\u00ec\u00ef\59\35\2\u00ed\u00ef\7a\2\2\u00ee"+
		"\u00eb\3\2\2\2\u00ee\u00ec\3\2\2\2\u00ee\u00ed\3\2\2\2\u00ef\u00f2\3\2"+
		"\2\2\u00f0\u00ee\3\2\2\2\u00f0\u00f1\3\2\2\2\u00f18\3\2\2\2\u00f2\u00f0"+
		"\3\2\2\2\u00f3\u00f6\5=\37\2\u00f4\u00f6\5;\36\2\u00f5\u00f3\3\2\2\2\u00f5"+
		"\u00f4\3\2\2\2\u00f6:\3\2\2\2\u00f7\u00f9\t\3\2\2\u00f8\u00f7\3\2\2\2"+
		"\u00f9\u00fa\3\2\2\2\u00fa\u00f8\3\2\2\2\u00fa\u00fb\3\2\2\2\u00fb<\3"+
		"\2\2\2\u00fc\u00fd\7\62\2\2\u00fd\u00ff\7z\2\2\u00fe\u00fc\3\2\2\2\u00fe"+
		"\u00ff\3\2\2\2\u00ff\u0101\3\2\2\2\u0100\u0102\t\4\2\2\u0101\u0100\3\2"+
		"\2\2\u0102\u0103\3\2\2\2\u0103\u0101\3\2\2\2\u0103\u0104\3\2\2\2\u0104"+
		">\3\2\2\2\u0105\u0107\t\5\2\2\u0106\u0105\3\2\2\2\u0107\u0108\3\2\2\2"+
		"\u0108\u0106\3\2\2\2\u0108\u0109\3\2\2\2\u0109@\3\2\2\2\u010a\u010c\7"+
		"\17\2\2\u010b\u010a\3\2\2\2\u010b\u010c\3\2\2\2\u010c\u010d\3\2\2\2\u010d"+
		"\u010e\7\f\2\2\u010e\u010f\3\2\2\2\u010f\u0110\b!\2\2\u0110B\3\2\2\2\u0111"+
		"\u0113\7\"\2\2\u0112\u0111\3\2\2\2\u0113\u0114\3\2\2\2\u0114\u0112\3\2"+
		"\2\2\u0114\u0115\3\2\2\2\u0115\u0116\3\2\2\2\u0116\u0117\b\"\2\2\u0117"+
		"D\3\2\2\2\u0118\u0119\7\61\2\2\u0119\u011a\7\61\2\2\u011a\u011e\3\2\2"+
		"\2\u011b\u011d\n\6\2\2\u011c\u011b\3\2\2\2\u011d\u0120\3\2\2\2\u011e\u011c"+
		"\3\2\2\2\u011e\u011f\3\2\2\2\u011f\u0121\3\2\2\2\u0120\u011e\3\2\2\2\u0121"+
		"\u0122\b#\2\2\u0122F\3\2\2\2\u0123\u0124\7-\2\2\u0124H\3\2\2\2\u0125\u0126"+
		"\7/\2\2\u0126J\3\2\2\2\u0127\u0128\7,\2\2\u0128L\3\2\2\2\u0129\u012a\7"+
		"\61\2\2\u012aN\3\2\2\2\u012b\u012c\7\'\2\2\u012cP\3\2\2\2\u012d\u012e"+
		"\7>\2\2\u012e\u012f\7>\2\2\u012fR\3\2\2\2\u0130\u0131\7@\2\2\u0131\u0132"+
		"\7@\2\2\u0132T\3\2\2\2\u0133\u0134\7@\2\2\u0134\u0135\7@\2\2\u0135\u0136"+
		"\7@\2\2\u0136V\3\2\2\2\u0137\u0138\7(\2\2\u0138X\3\2\2\2\u0139\u013a\7"+
		"~\2\2\u013aZ\3\2\2\2\u013b\u013c\7z\2\2\u013c\u013d\7q\2\2\u013d\u013e"+
		"\7t\2\2\u013e\\\3\2\2\2\u013f\u0140\7?\2\2\u0140^\3\2\2\2\u0141\u0142"+
		"\7>\2\2\u0142\u0143\7@\2\2\u0143`\3\2\2\2\u0144\u0145\7>\2\2\u0145b\3"+
		"\2\2\2\u0146\u0147\7>\2\2\u0147\u0148\7?\2\2\u0148d\3\2\2\2\u0149\u014a"+
		"\7\u0080\2\2\u014af\3\2\2\2\23\2\u00d7\u00df\u00e5\u00e9\u00ee\u00f0\u00f5"+
		"\u00fa\u00fe\u0101\u0103\u0106\u0108\u010b\u0114\u011e\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}