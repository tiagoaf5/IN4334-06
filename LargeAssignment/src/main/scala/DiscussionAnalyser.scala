import java.io.PrintWriter
import java.text.SimpleDateFormat
import java.time.{ZoneId, LocalDate}
import java.time.format.DateTimeFormatter

import ch.usi.inf.reveal.parsing.artifact.{StackOverflowComment, StackOverflowArtifact, StackOverflowUser}
import ch.usi.inf.reveal.parsing.model.java.JavaASTNode
import ch.usi.inf.reveal.parsing.model.json.JsonASTNode
import ch.usi.inf.reveal.parsing.model.stacktraces.StackTraceASTNode
import ch.usi.inf.reveal.parsing.model.xml.XmlASTNode
import ch.usi.inf.reveal.parsing.model.{CommentNode, TextFragmentNode}
import ch.usi.inf.reveal.parsing.units.{TextReadabilityMetaInformation, CodeTaggedUnit, InformationUnit, NaturalLanguageTaggedUnit}

import scala.collection.immutable.HashSet
import scala.collection.mutable

/**
  * Created by luiscleto on 08/12/2015.
  */
class DiscussionAnalyser(filedir: String, filename: String, tagFilters: Seq[String], localTags: LocalTagBank, exclusiveTags: Boolean) {

  ///stores aggregated data for a discussion's answers
  class AnswersProperties(val max_score: Int, val avg_score: Double, val min_score: Int, val max_length: Int,
                          val avg_length: Double, val min_length: Int)
  class InformationUnitsProperties(val code_p: Double, val java_p: Double, val json_p: Double, val xml_p: Double, val stack_traces_p: Double,
                                   val total_length: Int, val words_count: Int, val intercalations: Int, val text_speak_count: Int, val urls_count: Int)
  class CommentsProperties(val max_length: Int, val avg_length: Double, val min_length: Int)

  val daysOfWeek = List("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  var numFiles = 0

  val textSpeakDictionary: Set[String] = new HashSet() ++ Set("a$$","a&f","a'ight","a.i.m.","a/l","a/m","a/s/l","a/s/l/p","a/s/l/r","a1t","a3","aaaaa","aabf","aaf","aak","aamof","aatf","aatw","abd","abend","abft","aboot","abreev","absnt","abt","abwt","acc","acct","acgaf","ack","addy","adhd","adl","admin","adn","aeap","af","afaiaa","afaic","afaicr","afaics","afaict","afaik","afair","afaiu","afc","afcpmgo","afg","afk","afkb","agn","ah","ahole","ai","aiadw","aiamu","aicmfp","aight","aightz","aiic","aiid","aiight","aim","ain't","aite","aitr","aiui","aiws","ajax","aka","akp","akpcep","alaytm","alol","alot","alotbsol","alright","alrite","alrt","alryt","ama","amf","amiic","amiigaf","aml","amsp","anim8","anl","anlsx","anon","anuda","anw","anwwi","any1","anywaz","aob","aoc","aoe","aon","aos","aota","aoto","aoys","api","apoc","apod","app","appt","aprece8","apreci8","apu","aqap","ar","arnd","arse","arsed","arvo","asafp","asaik","asap","asarbambtaa","asbmaetp","ase","asf","ashl","ashole","asic","asl","aslo","aslop","aslp","aslr","aslrp","asr","asshle","atb","atfp","atl","atm","ato","atop","atp","atq","atst","attn","attotp","atw","aty","audy","aufm","aufsm","aup","av7x","avgn","avie","avsb","avtr","avvie","avy","awb","awes","awk","awol","awsic","awsm","awsome","ayagob","ayb","aybab2m","aybab2u","aybabtg","aybabtu","ayc","ayd","aydy","ayec","ayfk","ayfkm","ayfr","ayfs","ayk","aykm","ayl","aymf","ayok","aypi","aypi","ays","aysm","ayst","ayt","ayte","aytf","ayty","ayw","azhol","azn","azz",
    "bi","b&","b'day","b-cuz","b-day","b.f.f.","b.s.","b/c","b/cos","b/g","b/s/l","b/t","b/w","b00n","b00t","b0rked","b1tch","b2b","b2u","b2w","b3","b4","b4n","b4u","b4ug","b4ul","b8","b82rez","b8rez","b@","bab","babi","baf","baggkyko","bah","bai","bak","bakk","balz","bamf","bamofo","bau","bb","bb4h","bb4n","bbbj","bbe","bbf","bbfn","bbfs","bbfu","bbg","bbi","bbiab","bbiaf","bbialb","bbiam","bbias","bbiaw","bbifs","bbilb","bbilfm","bbim","bbk","bbl","bbl8a","bblig","bbm","bbml","bbn","bbol","bbp","bbq","bbrs","bbs","bbsts","bbt","bbtn","bbvl","bbw","bbwb","bbwe","bbwl","bby","bbz","bc","bch","bck","bcnu","bcnul8r","bcoz","bcurl8","bcuz","bd","bday","bdfl","be4","beatch","bebe","becuse","becuz","beech","beeoch","beezy","beotch","besos","bestie","betch","betcha","bettr","bewb","bewbs","bewbz","bewt","beyatch","beyotch","bezzie","bf","bf's","bf+gf","bf4e","bf4eva","bf4l","bfam","bfd","bfe","bff","bffa","bffaa","bffae","bffaw","bffe","bffeae","bffene","bffl","bffn","bfftddup","bfg","bfh","bfhd","bfitww","bfn","bfs","bft","bg","bh","bhwu","biab","biach","biaf","biatch","bibi","bibifn","bicbw","bich","bigd","bii","bilf","bilu","bion","biotch","bioya","bish","bitd","biw","biwm","biz","bizatch","bizi","biznatch","biznitch","bizzle","bj","bk","bka","bl","bleme","bleve","blg","blh","bling-bling","blj","bljb","blk","blkm","blnt","blog","blogger","blu","bm","bm&y","bm4l","bma","bmay","bmf","bmfe","bmfl","bmha","bml","bmoc","bmttveot","bmvp","bn","bndm3ovr","bng","bnib","bnol","bnr","bo","boati","bobfoc","bobw","boffum","bofh","bogo","bogof","bogsatt","bohic","bohica","boi","bol","bonr","boomm","bord","bos","botoh","bout","bovered","bowt","boxor","bpot","br","brah","brb","brbbrb","brbf","brbg2p","brbigtp","brbl","brbmf","brbn2gbr","brbs","brbts","brd","brfb","brgds","brh","bro","bros","broseph","brover","brt","bruh","bruhh","bruv","bruva","bruz","bs","bsmfh","bsod","bsomn","bstfu","bstrd","bsx","bsxc","bt","btb","btch","btcn","btd","btdt","btdtgtts","btfl","btfo","btfw","btias","btm","btr","bts","btsoom","bttt","bttyl","btw","btwilu","btwitiailwu","btwn","bty","bubar","bubi","budzecks","buhbi","bukket","bur","burma","buszay","but6","butsecks","butterface","buwu","bw3","bwim","bwoc","bwpwap","bwt","byak","byeas","byes","bykt","byob","byoc","byoh","byself","bytabm","bytch","bz","bzns","bzy","bzzy",
    "c","c 2 c","c&c","c'mon","c-p","c.y.a","c/b","c/t","c14n","c2","c2c","c2tc","c4ashg","c@","cam","cancer stick","catwot","cawk","cayc","cb","cba","cbb","cbf","cbfa","cbfed","cbi","ccl","ccna","cd9","celly","cex","cexy","cfas","cfid","cfm","cg","cgad","cgaf","cgf","ch@","champs","char","cheezburger","chik","chilax","chillax","chillin","chk","chohw","chr","chronic","chswm","chswmrn","chu","chut","cid","cig","cigs","cihswu","cihyn","cilf","cing","cis","ciwwaf","cless","clm","clt","cluebie","cm","cma","cmao","cmar","cmb","cmbo","cmcp","cmeo","cmh","cmiiw","cmitm","cml","cml8r","cmliuw2","cmomc","cmon","cmplcdd","cmplte","cmptr","cms","cmt","cmw","cn","cnc","cnt","cob","cod","cod4","cod5","codbo","codbo2","code 29","code 8","code 9","code9","cof","coiwta","col","comin'","comnt","comp","compy","congrats","contrib","contribs","convo","coo","cood","copyvio","cos","cotf","cotm","cowboy choker","coz","cp","cpl","cpm","cptn","cpu","cpy","cr","cr8","crakalakin","crazn","cre8or","crm","crp","crs","crunk","crzy","cs","cs:s","csi","cskr","csl","ct","ctc","ctf","ctfd","ctfo","ctfu","ctm","ctn","ctnbos","ctncl","ctpc","ctpos","ctrl","ctrn","cts","ctt","cu","cu2nit","cu46","cubi","cud","cuic","cul","cul83r","cul8er","cul8r","cul8tr","culd","cunt","cuom","cuple","curn","cut3","cuwul","cuz","cuzz","cvq","cw2cu","cwd","cwm","cwmaos","cwot","cwtgypo","cwyl","cya","cyal","cyal8r","cyas","cyb","cybl","cybr","cybseckz","cye","cyff","cyl","cyl,a","cyl8","cyl8er","cylbd","cylor","cym","cyntott","cyt","cyu","c|n>k",
    "d&c","d&df","d.t.f","d.w","d/c","d/l","d/m","d/w","d00d","d1ck","d2","d2m","d2t","d8","da","da2","dadt","dafs","dah","daii","damhik","damhikijk","damhikt","dass","dat","dats","dawg","dayum","dayumm","db","db4l","dbab","dbafwtt","dbag","dbeyr","dbg","dbh","dbi","dbm","dbz","dc","dc'd","dctnry","dcw","dd","ddf","ddg","ddl","ddos","ddr","ded","deets","deez","def","defs","degmt","dem","der","dernoe","detai","dewd","dey","df","dfc","dfo","dftba","dftc","dfu","dfw","dfw/m","dfwm","dfwmt","dg","dga","dgac","dgaf","dgara","dgas","dgms","dgoai","dgt","dgypiab","dh","dhac","dhcp","dhly","dhv","diacf","diaf","diah","dic","dick","diez","diff","dih","dikhed","diku","diky","dil","dilf","dillic","dillifc","dilligad","dilligaf","dilligas","din","din't","dirl","dis","dit","diy","dju","dk","dkdc","dl","dlf","dlibu","dln","dm","dmaf","dmba*","dmi","dmn","dmu","dmwm","dmy","dn","dnd","dndp","dnimb","dno","dnrta","dnrtfa","dns","dnt","dnw","doa","dob","dod","dogg","doin","doin'","don","doncha","donno","dont","dontcha","dood","doodz","dos","dotc","doypov","dp","dpmo","dprsd","dqmot","dqydj","dr00d","drc","drm","drood","dsided","dsu","dt","dta","dtb","dth","dtl","dtp","dtrt","dts","dttm","dttml","dttpou","dttriaa","du2h","ducy","dugi","dugt","duk","dulm","dum","dun","dunna","dunno","duno","dupe","dutma","dvda","dw","dwai","dwb","dwbh","dwbi","dwi","dwioyot","dwmt","dwn","dwt","dwy","dy2h","dya","dyac","dycotfc","dyec","dygtp","dyk","dylh","dylm","dylos","dym","dynk","dynm","dyt","dyth","dyw","dyw2gwm","dywtmusw",
    "e-ok","e.g.","e4u2s","eabod","ead","ebitda","ecf","edumacation","eedyat","eejit","ef","ef-ing","efct","effed","efffl","effin","effing","eg","ehlp","eil","el!t","eleo","ello","elo","em","emm","emo","emp","enat","enit","enof","enuf","enuff","eob","eoc","eod","eof","eom","eos","eot","eotw","epa","eq","eq2","ere","errythin","esad","esadyffb","esbm","esc","esl","eta","etla","etmda","etp","eula","ev1","eva","evaa","evar","evercrack","every1","evn","evr","evry","evry1","evrytin","ex-bf","ex-gf","exp","ey","eyez","ez","ezi",
    "f u","f#cking","f&e","f'n","f-ing","f.b.","f.m.l.","f.u.","f/o","f00k","f2f","f2p","f2t","f4c3","f4eaa","f4f","f4m","f8","f9","f@","fa-q","faa","fab","faggit","fah","faic","fam","fankle","fao","fap","fapping","faq","farg","fashizzle","fav","fave","fawk","fbimcl","fbk","fbtw","fc","fcbk","fcfs","fck","fckd","fckin","fcking","fckm3hdbayb","fcku","fcol","fcuk","fe","feat","feck","fer","ferr","ff","ffa","ffcl","ffr","ffs","fft","ffxi","fg","fgi","fgs","fgssu","fgt","fi","fi9","fibijar","fifo","fify","figjam","figmo","fiic","fiik","fimh","fio","fitb","fiv","fk","fka","fkd","fker","fkin","fking","fkn","fku","flamer","flames","flicks","floabt","fm","fmah","fmao","fmb","fmbb","fmbo","fmfl","fmflth","fmh","fmhb","fmi","fmir","fmita","fml","fmltwia","fmn","fmnb","fmnkml","fmph","fmq","fmr","fmsh","fmth","fmuta","fmutp","fn","fnar","fnci","fnny","fnpr","fny","fo","fo shizzle","fo sho","foa","foad","foaf","foah","fob","focl","fofl","foia","fol","folo","fomofo","fone","foo","foobar","foocl","fook","for sheeze","fos","foshizzle","fosho","foss","fotcl","fotm","fouc","fov","foyb","fo`","fp","fpmitap","fpos","fps","frag","fragged","fren","frens","frgt","fri.","friggin","frk","frm","frnd","frnds","fs","fsho","fsm","fsob","fsod","fsr","fst","ft","ft2t","fta","ftb","ftbfs","ftf","ftfa","ftfw","ftfy","ftio","ftk","ftl","ftlog","ftlt","ftmfw","ftmp","ftp","ftr","fts","fttp","ftw","fu","fua","fuah","fub","fubah","fubalm","fubar","fubb","fubh","fubohic","fubr","fucken","fucktard","fuctard","fud","fudh","fudie","fugly","fuh-q","fuhget","fuk","fukin","fukk","fukkin","fukn","fukr","fulla","fumfer","funee","funner","funy","fuq","fus","fut","fuu","fux","fuxing","fuxor","fuxored","fvck","fwb","fwd","fwiw","fwm","fwob","fwp","fxe","fxp","fy","fya","fyad","fyah","fyb","fyc","fye","fyeo","fyf","fyfi","fyi","fyk","fyl","fym","fyp","fyrb","fytd",
    "*g*","g'nite","g/f","g/g","g0","g00g13","g1","g2","g2/-/","g2bg","g2bl8","g2cu","g2e","g2g","g2g2tb","g2g2w","g2g4aw","g2gb","g2gb2wn","g2ge","g2gn","g2gp","g2gpc","g2gpp","g2gs","g2h","g2hb","g2k","g2p","g2t2s","g3y","g4u","g4y","g8","g9","g@y","ga","gaalma","gaf","gafi","gafl","gafm","gagf","gagp","gah","gai","gaj","gal","gamez","gangsta","gank","gaoep","gaw","gawd","gb","gb2","gba","gbioua","gbnf","gbtw","gbu","gby","gcad","gcf","gd","gd&r","gd4u","gday","gdby","gded","gdgd","gdi","gdiaf","gdih","gdilf","gdmfpos","gdr","gemo","getcha","geto","gewd","gey","gf","gfad","gfadh","gfak","gfam","gfar2cu","gfas","gfd","gfe","gfe2e","gfg","gfgi","gfi","gfj","gfl","gfo","gfu","gfurs","gfus","gfx","gfy","gfyd","gfym","gfys","gg","gga","ggal","ggf","ggg","ggi","ggnore","ggp","ggpaw","ggs","gh","ghei","ghey","gigig","gigo","gilf","gim","gimme","gimmie","gir","gis","gitar","giv","giyf","gj","gjial","gjp","gjsu","gjt","gky","gkys","gl","gl hf","gl&hf","gla","glbt","glf","glhf","glln","glnhf","glty","glu","glu2","glux","glwt","gm","gma","gmab","gmabj","gmafb","gmao","gmfao","gmilf","gmod","gmta","gmtyt","gmv","gmybs","gn","gn8","gnasd","gndn","gnfpwlbn","gng","gng2","gngbng","gnight","gnite","gnn","gno","gnoc","gnos","gnr","gnrn","gnst","gnstdltbbb","goc","goi","goia","goin","gok","gokid","gokw","gol","gomb","goml","gona","gonna","good9","gooh","goomh","gork","gosad","gotc","gotcha","gotta","gow","goya","goyhh","gp","gpb","gpwm","gpytfaht","gr8","gr8t","grats","gratz","grfx","grillz","grl","grmbl","grog","grrl","grtg","grvy","gsad","gsave","gsd","gsfg","gsi","gsoh","gsp","gsta","gt","gta","gtas","gtb","gtf","gtfa","gtfbtw","gtfh","gtfo","gtfoi","gtfon","gtfooh","gtfoomf","gtfu","gtfuotb","gtg","gtgb","gtgbb","gtgfn","gtglyb","gtgmmiloms","gtgn","gtgp","gtgpp","gtgtb","gtgtpirio","gtgtwn","gth","gtha","gthb","gthmf","gtho","gthu","gthyfah","gtk","gtm","gtn","gtp","gtr","gts","gtsy","gttp","gtty","gu","gu2i","gud","gudd","gui","gurl","gurlz","guru","gw","gwijd","gwm","gwork","gwrk","gws","gwytose","gy","gyal","gypo","roflmfao",
    "h&k","h*r","h+k","h.o","h/e","h/mo","h/o","h/u","h/w","h2","h2gtb","h2o","h2sys","h3y","h4kz0r5","h4x","h4x0r","h4xor","h4xr","h4xrz","h4xx0rz","h4xxor","h8","h80r","h82sit","h83r","h8ed","h8er","h8r","h8red","h8s","h8t","h8t0r","h8t3r","h8te","h8tr","h8u","h9","habt","hafta","hagd","hagl","hagn","hago","hags","hai","hait","hak","hakas","hammrd","han","hau","hav","havnt","hawf","hawt","hawtie","hax","hax0r","hax0red","hax0rz","haxer","haxor","haxoring","haxors","haxorz","haxxor","haxxzor","haxz0r","haxzor","hayd","hb","hb4b","hbd","hbic","hbii","hbu","hby","hc","hcbt1","hcib","hcihy","hdop","hdu","hdydi","hdydt","heh","hella","heya","heyt","heyy","heyya","hf","hfn","hfs","hfsbm","hfwt","hg","hght","hhiad","hhiadb","hhok","hhyb","hi2u","hi2u2","hiet","hiik","hijack","hith","hiw","hiya","hiybbprqag","hj","hl","hl2","hla","hlb","hld","hldn","hldon","hll","hlm","hlo","hlp","hly","hlysht","hmb","hmewrk","hmfic","hml","hmoj","hmu","hmul","hmus","hmw","hmwk","hmwrk","hng","hngry","hnic","ho","hoas","hoay","hoh","hom","homey","homie","homo","hoopty","hott","howdey","howz","hpb","hpoa","hppy","hpy","hpybdy","hr","hre","hrny","hrs","hru","hrud","hs","hsd","hsik","hsr","hss","hswm","ht","htc","htf","htfu","hth","hthu","htr","http","hu","hubby","hud","huggle","hugz","hun","hv","hve","hvnt","hw","hw/hw","hwg","hwga","hwik","hwk","hwmbo","hwmnbn","hwms","hwu","hwz","hxc","hy","hyb","hyg","hyk",
    "i <3 u","i c","i8","i8u","i<3 u","i<3u","iab","iafh","iafi","iag","iah","iai","ianabs","ianacl","ianal","ianalb","ianars","ians","ianyl","iap","iasb","iaspfm","iatb","iateu","iavb","iaw","iawtc","iawtp","iawy","ib","ibbl","ibcd","ibs","ibt","ibtl","ibw","ic","icb","icbi","icbiwoop","icbt","icbu","icbyst","iccl","icgup","icic","icp","icr","icsrg","ictrn","icty","icu","icudk","icup","icw","icwudt","icwum","icydk","icydn","icymi","id10t","idbtwdsat","idby","idc","iddi","idec","idek","idfc","idfk","idfts","idgac","idgad","idgaf","idgaff","idgafs","idgara","idgas","idgi","idjit","idk","idkbibt","idke","idkh","idkh2s","idkt","idkw","idkwiwdwu","idkwts","idkwurta","idkwym","idky","idkyb","idkymb2","idl","idli","idlu","idly","idlyitw","idm","idn","idnk","idno","idntk","idnwths","idonno","idop","idot","idr","idrc","idrfk","idrgaf","idrgaff","idrk","idrts","idsw","idtis","idtkso","idts","idunno","iduwym","idw2","idw2n","idwk","idwt","idwtg","idyat","iebkac","ietf","iff","ifhu","ifhy","ifk","iflu","ifthtb","ifttt","ifwis","ig","ig2g","ig5oi","igahp","igalboc","igg","ight","igkymfa","igs","igt","igtg","igtgt","igtkya","igu","igyb","ih","ih2gp","ih2p","ih8","ih8mls","ih8p","ih8tu","ih8u","ih8usm","ih8y","ihac","ihat3u","ihistr","ihiwydt","ihml","ihmp","ihnc","ihnfc","ihni","iht","ihtfp","ihtgttbwijd","ihtp","ihtsm","ihtutbr","ihu","ihurg","ihusb","ihusfm","ihusm","ihy","ihya","ihysm","ihysmrn","iigh","iight","iiok","iirc","iistgtbtipi","iit","iitywimwybmad","iitywybmad","iiuc","iiw2","iiwii","ij","ijaf","ijcomk","ijdk","ijdl","ijeomk","ijf","ijgl","ijit","ijk","ijp","ijpmp","ijpms","ijr","ijsabomomcibstg","ik","iki","ikic","ikm","ikr","ikt","ikwud","ikwum","ikwyl","ikwym","ilbbaicnl","ilbcnu","ilcul8r","ilhsm","ili","ilk2fku","ilml","ilms","ilotibinlirl","ilshipmp","iltf","iltwymmf","ilu","ilu2","iluaaf","ilulafklc","ilum","ilusfm","ilusm","iluvm","iluvu","iluvya","iluwamh","ilvu","ily","ily2","ily4e","ily4ev","ilyaas","ilyal","ilyb","ilybby","ilybtid","ilyf","ilygsm","ilykthnxbai","ilyl","ilylab","ilylabf","ilylafklc","ilylas","ilylc","ilym","ilymtyk","ilymtylm","ilysfm","ilysfmb","ilysm","ilysmih","ilysmm","ilysmydek","ilysvm","ilyvm","ilywamh","im","im'd","im26c4u","ima","imao","imb","imcdo","imed","imfao","imfo","imh","imhbco","imhe","imho","imm","imma","imnerho","imnl","imnshmfo","imnsho","imo","imoo","impo","impov","imsb","imsry","imtaw","imts","imu","imusm","imvho","imwtk","imy","imy2","imya","imysfm","in2","inb4","inbd","incld","incrse","ind2p","indie","inef","inet","inh","inho","inhwh","init","inmp","innit","ino","instagib","instakill","intarwebs","intel","interweb","intpftpotm","inttwmf","invu","ioh","iois","iokiya","ionno","iono","iotd","iou","iow","ioya","ioyk","ip","irc","irdc","irdgaf","irdk","irgtgbtw","irhtgttbr","irhy","irl","irly","irt","irtf","is2g","isb","isbya","isd","ise","isfly","isg","ishii","isianmtu","isj","iso","isp","iss","istg","istr","istwfn","iswydt","ita","itb","itc","itd","ite","itk","itn","itt","ityltk","itys","itz","itzk","iucmd","iukwim","iunno","iuno","ive","iw2f","iw2fu","iw2mu","iwaa","iwbrbl@r","iwc","iwfusb","iwfy","iwfybo","iwg","iwhi","iwhswu","iwjk","iwk","iwlu4e","iwmu","iwmy","iws","iwsn","iwsul8r","iwtfu","iwtfy","iwthswy","iwtly","iwu","iwuwh","iwy","iwyb","iwyn","iwythmb","iyam","iyc","iyd","iydhantsdsaaa","iydmma","iyf","iyflg","iygm","iykwim","iym","iyo","iyq","iyss","iyswim","iywt","iz",
    "j-c","j/a","j/c","j/j","j/k","j/o","j/p","j/s","j/t","j/w","j00","j00r","j2bs","j2c","j2f","j2luk","j2lyk","j4f","j4g","j4l","j4u","jalaudlm","jas","jb","jbu","jc","jcam","jcath","jdfi","jebus","jeomk","jf","jfc","jfdi","jff","jfg","jfgi","jfi","jfj","jfk","jfl","jflts","jfn","jfo","jfr","jftr","jfu","jfwy","jg2h","jgiyn","jgtfooh","jh","jhm","jho","jic","jit","jizz","jj","jj/k","jja","jk","jka","jking","jkl","jklol","jkn","jks","jkz","jlma","jlt","jm","jma","jml","jmo","jms","jom","joo","jooc","jooce","joor","jp","js","jsa","jsing","jst","jsuk","jsun","jsut","jsyk","jsyn","jtay","jtbs","jtc","jtfo","jtluk","jtlyk","jtoi","jtol","jttsiowctw","jtty","jtumltk","jtwii","jtwiw","jtyltk","jtysk","jumping the couch","jus","juss","juz","juzt","jw","jw2k","jwas","jwtlyk","jyfihp",
    "k","k3wl","ka","kafn","kah","kaw","kay","kb","kcco","kek","kewel","kewl","kfc","khitbash","khuf","kia","kib","kic","kicks","kig","kiled","kinda","kir","kis","kisa","kit","kitfo","kitteh","kiu","kiwf","kk","kkk","kkthnxbye","kky","kl","km","kma","kmag","kmao","kmb","kmfa","kmhba","kml","kmn","kmp","kmsl","kmswl","knackered","knewb","knn","kno","knw","ko","kol","koo","kool","kos","kotc","kotl","kotor","kots","kpc","ks","kss","kssd","kt","ktc","ktfo","kthanxbi","kthnxbai","kthnxbye","kthx","kthxbai","kthxbi","kthxbye","kthxbye","kthxgb","kthxmn","kthz","ktnx","kuhl","kul","kute","kutgw","kuwl","kwik","kwim","kwis","kwit","kwiz","kwl","kwtsds","kyag","kyfag","kyfc","kyko","kyms","kys",
    "l0lz","l2","l2m","l2ms","l2p","l2r","l337","l33t","l4m3rz","l8","l84skool","l8a","l8er","l8ers","l8r","l8rs","l8rz","l8s","l8t","l8ta","l8ter","l8tr","l@u","laff","lafs","lak","lal","lalol","lam","lamf","lan","lappy","larp","lasb","lat","lata","lates","latn","latr","latwttb","lau","lawd","lawl","lawl'd","lawled","lawls","lawlz","lazer","lazor","lbh","lbnr","lbo","lbr","lbvs","lcsnpc","ldr","lee7","leet","legit","leik","leme","lemme","lesbo","less than 3","less than three","lez","lezbean","lezbo","lezzzie","lf","lf1m","lf2m","lfg","lfl","lfm","lfnar","lfp","lfr","lgb","lgbnaf","lgbtq","lgf","lggd","lgn","lgo","lgot","lgr","lgs","lhao","lhs","lhsrn","lic","liec","liek","liekz","lifo","ligaff","ligafs","ligas","lih","liita","lik","lil","lim","limh","liol","lirl","liu","liv","liyf","lj","lk","lke","llab","llap","llc","llf","llh","llol","lltnt","lm4aq","lma","lmamf","lmao","lmaol","lmaomtoaoa","lmaonade","lmaool","lmaootf","lmaorof","lmaorotf","lmaowrotf","lmaowtntpm","lmaoxh","lmap","lmb","lmbao","lmbfwao","lmbo","lmcao","lmclao","lmd","lmfao","lmfbo","lmffao","lmffo","lmfho","lmfo","lmfpo","lmfr","lmfto","lmg","lmgdao","lmgtfy","lmhao","lmho","lmip","lmirl","lmk","lmks","lmkwut","lml","lmmfao","lmmfaos","lmmfas","lmmffao","lmo","lmoao","lmp","lmpo","lms","lmsao","lmso","lmtd","lmtfa","lmto","lmtus","lmty","lmvo","ln","lnk","lobfl","lobl","lof","lofi","lofl","loflmao","loi","lol","lol'd","lol2u","lol@u","lolarotf","lolaw","lolbs","lolcano","lolci","lolcity","lold","lolees","lolerz","lolf","lolin","lolio","lollam","lollercaust","lollercoaster","lollerskates","lolm","loln","lolngs","lolocost","lolol","lololz","lolpimp","lolq","lolrof","lolrotf","lols","lolvq","lolwtime","lolz","lomg","loml","lomy","loomm","lorl","lorrl","lotf","loti","loto","lotr","lov","lovu","loxen","loxxen","lozer","lpb","lpiaw","lpms","lq","lq2m","lqtm","lqtms","lqts","lrfl","lrh","lrqtms","lrt","lsfw","lshic","lshid","lshipmp","lshismp","lshiwms","lshmson","lshrn","lsmih","lsr","lsudi","lt","ltb","lthtt","ltip","ltm","ltmq","ltms","ltnc","ltns","ltnsoh","ltnt","ltp","ltr","lttpot","ltw","ltywl","lu2","lu2d","lu4l","lub","luf","luff","lug","luk","lukin","lul","lulab","lulas","lulz","lumumi","lurker","lurve","luser","lusm","luv","luver","luvuvm","luvv","luzar","lv","lve","lvl","lvn","lvr","lvya","lwih","lwn","ly","ly2","lya","lyaab","lyaaf","lyao","lybo","lyf","lyfao","lyfe","lyk","lyk3","lyke","lyl","lylab","lylaba","lylad","lylafklc","lylam","lylas","lylasa","lylno","lyls","lymi","lysfm","lysm","lyt","lyvm","lzer","lzr",
    "m","m$","m$wxp","m&d","m'kay","m.i.a","m.o","m/b","m/f","m2","m3","m473s","m473z","m4f","m4m","m8","m84l","m8s","m8t","m8t's","m9","mabby","mabe","mah","mai","mao","marvy","masterb8","mastrb8","mayb","mayte","mb","mbf","mbfal","mbhsm","mbl8r","mcds","mcs","mcse","me2","meatcurtain","meatspace","meeh","mego","meh","messg","mf","mf2f4sx","mfa","mfah","mfao","mfb","mfer","mfg","mfkr","mflfs","mfr","mfw","mgiwjsdchmw","mgmt","mhh","mhm","mho","mia","mic","miid","milf","miltf","min","mins","miq","mir","mirl","misc.","miself","mite","miw","miwnlf","mk","mkay","mlc","mle","mlia","mlod","mlp","mmamp","mmas","mmatc","mmatp","mmbocmb","mmd","mmiw","mmk","mml","mml8r","mmlfs","mmmkay","mmo","mmt","mmtyh","mmw","mngmt","mngr","mnm","mnt","mobo","mof","mofo","moh","mohaa","mol","mompl","moobs","mor","morf","moro","mos","moss","motarded","motd","motos","mpaw","mpbis","mpd","mpgis","mph","mpih","mpty","mrau","msf","msg","msgs","msh","msibo","msie","msm","msmd","msngr","mssg","mstrb8r","msv","mtc","mtf","mtfbwu","mtfbwy","mtg","mtherfker","mthrfkr","mtl","mtr","mtrfkr","mty","mu","mudda","mul","musiq","musm","mutha","muve","muvva","muzik","mw2","mw3","mwah","mwf","mwsmirl","myaly","myfb","myke","myn","myob","myodb","myofb","mypl","mysm","myspce",
    "mmorpg","n","n e","n/a","n/a/s/l","n/c","n/m","n/n","n/o","n/t","n00b","n00bs","n00dz","n00s","n1","n199312","n1994","n2","n2b","n2bb","n2br","n2g","n2m","n2mh","n2mhbu","n2mhjc","n2mu","n2n","n2p","n64","n8v","na","na4w","naa","nade","nafc","nafkam","naft","nafta","nah","namh","nao","natch","natm","naw","naw-t","nawidt","nawt","naww","nayl","nb","nb,p","nba","nbd","nbdy","nbf","nc","ncaa","ncs","nd","ndit","ndn","nds","ne","ne1","neday","nedn","nefing","negl","nei","neida","nekkid","nemore","nes","nethin","nething","neva","nevah","nevar","nevarz","nevm","nevr","newais","neway","neways","newayz","newb","newbie","newez","nf","nfbsk","nfc","nfd","nff","nfi","nfr","nfs","nft","nfw","ng","ngaf","ngl","nh","nhatm","ni","ni994","nib","nic","nif","nifoc","nifok","nigysob","nimby","nin","nip","nips","nite","nizzle","nj","njoy","njp","nk","nkt","nld","nm","nm u","nmbr","nme","nmf","nmfp","nmh","nmhau","nmhm","nmhu","nmhwby","nmjb","nmjc","nmjch","nmjcu","nmjdhw","nmjfa","nmnhnlm","nmp","nmu","nmw","nmwh","nn","nn2r","nnaa","nnfaa","nnr","nntr","nntst","no pro","no1","noaa","noc","noe","noes","nofi","nolm","nomw","noob","noobie","nooblet","noobz0r","noodz","nookie","nop","norwich","notin","noty","noub","nowai","nowin","noyb","noygdb","np","np4np","npa","npc","npe","npgf","nph","npi","npnt","nps","nq","nr","nr4u","nrg","nrn","ns","nsa","nsas","nsfmf","nsfu","nsfw","nss","nst","nstaafl","nt","ntb","ntbn","nthg","nthin","nthn","ntigaf","ntk","ntkb","ntm","ntmk","ntmu","ntmy","ntn","ntrly","nts","ntstt","ntt","ntta","nttawwt","nttiawwt","ntty","ntw","ntxt","nty","nu","nub","nuff","nuffin","nufin","nutin","nuttin","nv","nvm","nvmd","nvmdt","nvmt","nvr","nvrm","nvrmnd","nw","nwb","nwih","nwj","nwrus","nws","nwtf","nwy","nxt","ny1","nyc","nyf","nyp","nywy",
    "o","o rly","o&o","o.p.","o/y","oaoa","oar","oaw","obgjfioyo","obj","obl","obo","obtw","obv","obvi","occ","ocd","ocgg","od","oday","odg","odtaa","oe","oed","of10","ofc","ofcol","ofn","oftc","oftn","oftpc","ofwg","og","ogw","oh noes","oh noez","ohic","ohn","ohnoez","ohy","oibmpc","oic","oicic","oicu","oicwydt","oidia","oink","oiyd","oj","ojsu","ok","oll","olpc","omdg","omdz","omfd","omfg","omfgn","omfgsh","omfj","omfl","omfsm","omfwtg","omg","omg's","omgd","omgf","omgg","omgicfbi","omgih","omgihv2p","omginbd","omgn","omgny","omgosh","omgroflmao","omgsh","omgty","omgukk","omgwtf","omgwtfbbq","omgwtfhax","omgwtfit","omgwtfnipples","omgyg2bk","omgykkyb","omgz","omgzors","omhg","omj","ommfg","omt","omw","omwh","omwts","omy","onoez","onoz","onud","onyd","oob","oobl","ooc","oohm","oom","oomf","oomm","ooo","ootb","ootd","oow","ooym","op","orgy","orlsx","orly","orly","orpg","os","osbutctt","osd","osifgt","oslt","osy","ot","otc","otfcu","otfl","otflmao","otflmfao","otflol","otfp","otft","oti","otl","otoh","otp","ots","ott","otw","outa","ova","oways","owned","ownt","ownz","ownzer","ownzorz","owt","oww","oyfe","oyid","oyo","oyr",
    "p-nis","p.o.b.","p.o.s","p.o.s.","p/oed","p/w","p00p","p0wn","p2p","p2w","p33n","p3n0r","p3n15","p3n1s","p4p","p911","p@w","pach","pachs","pae","pag","pah","parnts","pas","pasii","patd","paw","pb","pb&j","pbb","pbcakb","pbj","pbjt","pbkc","pbly","pbm","pbp","pcbd","pce","pcent","pcm","pco","pcrs","pda","pdg","pdq","peanus","pearoast","pebcak","pebkac","pebmac","peep dis","peeps","pen0r","pen15","penor","peoples","perv","pewp","pex","pezzas","pf","pfa","pfm","pfo","pfos","pfy","pg","ph#","ph33r","ph34r","phag","phail","phat","phayl","phear","phlr","phm","phq","phreak","phreaker","phuck","phucker","phuk","phun","phux","phuxor","piab","pic","piccies","pics","pihb","piihb","piitb","pima","pimfa","pimha","pimpl","pino","pir","pirlos","pita","pitfa","pitr","pitrtul","piw","pix","pk","pkemon","pker","pking","pl","pl0x","pl8","plac","plams","plars","platcs","ple's","pleaz","pleez","pleeze","pleze","pliz","plma","plmk","plocks","plom","plomb","ploms","plos","plox","ploxxorz","pls","plse","plx","plywm","plz","plzkthx","plzthx","pmfji","pmfsl","pmg","pmita","pmitap","pml","pmo","pmp","pmpl","pmsfl","pmsl","pmt","pnbf","pnhlgd","pns","pnus","po","po po","po'd","pob","poc","poed","poets","poi","poidnh","pol","poms","poo","poontang","pooter","popo","poq","pos","poscs","posmbri","potc","pots","pov","pow","pp","ppl","ppls","pplz","ppor","ppppppp","pr0","pr0n","pr0nz","prblm","prd","preggers","prego","prfct","prn","prncpl","prncss","prnoscrn","pro","prob","probly","probz","prod","prolly","prollz","promos","pron","proxie","prp","prsn","prty","prv","prvrt","prw","ps1","ps2","ps3","psa","psbms","psn","psos","psp","pssy","pst","pt33n","ptbb","ptfo","pthc","ptl","pto","ptw","puh-leaze","purty","puter","pvp","pvt","pw","pwb","pwcb","pwd","pwn","pwn3d","pwn3r","pwnage","pwnd","pwned","pwner","pwnt","pwnz","pwnzor","pwob","pwoms","pwor","pwos","pww","pxr","pydim","pyfco","pyt","pz","pzled","p^s",
    "q2c","q33r","q4u","qed","qfe","qfmft","qft","qft&gj","ql","qltm","qna","qool","qoolz","qotd","qotsa","qoty","qpr","qpwd","qq","qt","qt3.14","qte","qtpi",
    "r","r-tard","r.i.p","r.i.p.","r0x0rz","r2f","r8","r8p","r8pist","r8t","ra2","ra3","raoflmao","rawk","rawks","rawr","rb@u","rbau","rbay","rbm","rbtl","rbty","rcks","rcsa","rcvd","rdy","re","re/rehi","reefer","refl","rehi","rele","rents","rentz","rep","reppin","retrotextual","rff","rflmao","rfn","rgr","rhcp","rhgir","rhs","ricl","rifk","rihad","rino","rite","ritjive","rjct","rl","rlbf","rlf","rlg","rlgf","rlly","rln","rly","rlz","rlze","rm","rme","rmr","rmso","rn","rnt","ro","rockr","rodger","rofalol","rofc","roffle","roffle out loud","rofflecake","rofflecopters","roffleol","roffles","rofflmfao","rofl","rofl&pmp","roflao","roflastc","roflcopter","roflcopters","roflkmd","rofllh","roflmao","roflmaoapimp","roflmaool","roflmaopmp","roflmaouts","roflmaowpimp","roflmbfao","roflmbo","roflmfaopimp","roflmfaopmp","roflmgao","roflmgdao","roflmgdmfao","roflmgo","roflmho","roflmiaha","roflmmfao","roflol","roflolbag","roflpimp","roflpmp","roflwtime","rofpml","rofwl","roger","rogl","roglmfao","roi","roids","roj","rol","rolmao","rolmfao","rombl","rong","roofles","ror","rotf","rotfalol","rotffl","rotfflmao","rotfflmfao","rotfl","rotflaviab","rotflmao","rotflmaofaktd","rotflmaool","rotflmaostc","rotflmbo","rotflmfao","rotflmfaopimp","rotflmfaopmp","rotflmfho","rotflmho","rotflmmfao","rotflol","rotfpm","rotfwlmao","rotg","rotgl","rotglmao","rotw","rowyco","rox","roxor","roxorz","roxxor","rp","rpg","rpita","rplbk","rpo","rq","rr","rrb","rsn","rsp","rspct","rsps","rta","rtard","rtbq","rtf","rtfa","rtffp","rtfm","rtfmfm","rtfmm","rtfms","rtfp","rtfq","rtfs","rtfu","rtg","rtl","rtm","rtr","rtry","rts","ru","ru18","rua","ruabog","ruagoab","rubz2nt","rufkm","rugay","rugta","ruh","ruk","rukm","rumf","ruok","rur","rut","ruwm","rwb","ryt","ryte",
    "*s*","s'ok","s'pose","s'up","s.i.n.g.l.e","s.i.t.","s.o.a.b.","s.o.b.","s.w.a.k.","s/b","s2a","s2bu","s2r","s2u","s2us","s3x","s4se","s8ter","sab","sagn","sah","sahm","sase","sbc","sbcg4ap","sbd","sblai","sbrd","sbs","sbt","scnr","scool","scrilla","scrt","scurred","sd","sdf^","sdk","sdlc","sec","secks","secksea","secksy","sed","see through your eyes","seg","seks","sellin","seo","serp","sexc","sexe","sexi","sexii","sexilicious","sexx0rz","sez","sfam","sfe","sfh","sfipmp","sfm","sfr","sfs","sfsg","sfu","sfw","sfwuz","sfy","sg","sgb","sgbadq","sgi","sgtm","sh","shag","shawty","shd","shexi","shexy","shiat","shiet","shite","shiz","shizit","shiznat","shiznit","shizz","shizzle","shld","shmexy","shmily","sho","sho'nuff","showin","shrn","sht","shtf","shud","shuddup","shup","shure","shut^","shwr","shyat","shyt","siao","sibir","sic","sicl","sif","sifn't","sig","siggy","silf","simcl","simclmao","siol","sis","sista","sitb","sitmf","siu","siuya","sk","sk8","sk8er","sk8ing","sk8r","sk8ter","sk8tr","skb","sked","skeet","skewl","skhool","skillz","skl","skool","skoul","sktr","skwl","sl4n","sleepin","sleepn","slf","slgb","slng","slo","slore","slos","slp","slt","sl^t","sm","sm1","smb","smbd","smbt","smc","smd","smdb","smdvq","smeg","smexy","smf","smfd","smfpos","smh","smhb","smho","smithwaws","smofo","smst","smt","smthin","smthng","smtm","smto","smtoay","sn","snafu","snafubar","snes","snew","snf","snl","snm","snog","snogged","soa","soab","soad","soafb","sob","sobs","soc","soe","sof","sofas","sofs","soi","sok","sokay","sol","som'm","som1","somadn","some1","soml","soo","soobs","sool","sop","sorg","sorreh","sorta","sos","sosdd","sosg","sot","sotc","sotr","sowi","sowwy","soz","spk","spk2ul8r","sploits","sploitz","spos","sprm","sqtm","srch","srly","sroucks","srry","srs","srsly","srvis","sry","srynd2g","srzly","ss","ss4l","ssdd","ssdp","ssia","ssl","ssob","ssry","sssd","st","st1","st8","stb","stbx","stby","std","steamloller","stfd","stff","stfm","stfng","stfu","stfua","stfuah","stfub","stfuda","stfugbtw","stfun","stfuogtfo","stfuppercut","stfuyb","stfuysoab","stfw","stg","sth","sthing","sthu","stm","stoopid","stpd","str8","str8up","sts","stsp","stt","stufu","stupd","stw","stys","su","suabq","suagooml","suib","suk","suka","sukz","sul","sum1","sumfin","summin","sumone","sumthin'","sumtin","sup","supa","supposably","sus","susfu","sut","sutuct","sux","sux0rz","sux2bu","suxor","suxors","suxorz","suxx","suxxor","suyah","svn","svu","sw","swafk","swak","swakaah","swalk","swf","swm","swmbo","swmt","swp","swsw2b","swt","swtf","sx","sxc","sxcy","sxe","sxi","sxs","sxy","syatp","sydim","sydlm","syfm","syiab","syiaf","syl","syl8r","sym","syoa","syotbf","syrs","sys","sysop","syt","sytycd","syu","sz",
    "t#3","t,ftfy","t.t.y.l","t/a","t2b","t2m","t2u","t2ul","t2ul8r","t3h","t4a","t4m","t8st","ta","taci","tafn","taht","tai","taig","tal","tanq","tanstaafl","tard","tarfu","tat","tat2","tau","taunch","taw","tay","tb","tb4u","tba","tbc","tbd","tbf","tbfh","tbfu","tbh","tbhimo","tbnt","tbp","tbpfh","tbph","tbqf","tbqh","tbss","tbtfh","tbvh","tc","tcfc","tcfm","tcg","tchbo","tcial","tcoy","tcp","tcp/ip","td2m","tddup","tdf","tdl","tdtm","tdwdtg","te","teh","teotwawki","terd","tf2","tfa","tfb","tfbundy","tfc","tfd","tff","tfft","tffw","tfh","tfic","tfiik","tfl","tfln","tfm","tfs","tfta","tfti","tfu","tfu2baw","tg","tgfe","tgfitw","tgft","tgfu","tgfuap","tghig","tgif","tgiff","tgis","tgiwjo","tgsttttptct","tgtbt","tgwig","tgws","th@","tha","thankies","thankx","thanq","thanx","thar","thatz","thku","thn","thnk","thnx","tho","thot","thr","thr4","thru","tht","thwdi","thwy","thx","thxx","thz","ti2o","tia","tiafayh","tiai","tias","tiatwtcc","tif","tif2m","tifs","tifu","tigger","tiic","til","tilf","tinf","tinla","tinstaafl","tioli","tis","tisc","tisfu","tisg","tisly","tisnf","tiss","tisw","tiw","tix","tjb","tk","tk2ul","tkd","tker","tks","tku","tl","tl,dr","tl8r","tl:dr","tl; dr","tl;dr","tla","tlc","tld","tldnr","tldr","tlgo","tliwwv","tlk","tlk2me","tlk2ul8r","tlkin","tlkn","tltpr","tlyk","tma","tmaai","tmai","tmbi","tmi","tmk","tml","tmmrw","tmnt","tmo","tmoro","tmoz","tmr","tmr@ia","tmrrw","tmrw","tmrz","tms","tmsaisti","tmsg","tmsidk","tmth","tmtmo","tmtoyh","tmtt","tmw","tmwfi","tmz","tn1","tna","tnf","tnlnsl","tnx","tnxz","tob","tofy","toh","tok","tok2ul8r","tolol","tomm","tomoro","tomoz","tonite","tos","totes","totl","totm","totp","totpd","tou","toya","tp","tpb","tpiwwp","tps","tptb","tq","trani","tranny","trble","trd","trnsl8","trnsltr","troll","tru","ts","tsc","tsff","tsig","tsnf","tss","tstoac","tswc","tt4n","ttbc","ttbomk","ttc","ttfaf","ttfn","tthb","ttihlic","ttiuwiop","ttiuwop","ttiuwp","ttiwwop","ttiwwp","ttl","ttlly","ttly","ttm","ttml","ttmn","ttms","ttr","ttrf","tts","ttt","ttth","tttt","ttul","ttul8r","ttus","ttut","ttutt","tty","ttyab","ttyad","ttyal","ttyas","ttyiam","ttyitm","ttyl","ttyl8r","ttylo","ttylt","ttyn","ttyna","ttynl","ttynw","ttyo","ttyob","ttyotp","ttyrs","ttys","ttyt","ttytm","ttytt","ttyw","ttywl","tu","tuff","tuh","tut","tuvm","tv","tvm","tw","twajs","twat","twbc","twdah","twf","twfaf","twg","twi","twis","twoh","tws2wa","twss","twsy","twttr","twvsoy","twyl","twys","tx","txs","txt","txting","txtyl","ty","tyclos","tyfi","tyfn","tyfyc","tyfyt","tyl","tym","tyme","typ","typo","tyred","tys","tysfm","tysm","tysvm","tyt","tyto","tyty","tyvm","tyvvm",
    "u","u iz a 304","u'd","u'll","u'r","u'v","u've","u/l","u/n","u2","u2u","u4i","ua","uaaaa","uat","uayor","ub3r","uber","uctaodnt","udc","udek","uds","udwk","udy","ufab","ufia","ufic","ufmf","ufr","ugba","ugtr","uhab","uhems","ui","ujds","ukr","ukwim","ul","ulbom","umfriend","un2bo","un4rtun8ly","unt","uom","upcia","upia","upmo","upos","upw","ur","ur2g","ur6c","ura","uradrk","urafb","uraqt","urcrzy","ure","urg","urht","url8","urms","urmw","urnc","urs","ursab","ursdf","ursg","ursh","urssb","urstpid","urstu","urtb","urtbitw","urtrd","urtw","urw","uryyfm","usck","usd","ussr","usuk","usux","ut","ut","uta","utfs","utfse","utm","uttm","utube","utw","uty","uve","uvgtbsm","uw","uwc","uya","uyab",
    "v4g1n4","vag","vajayjay","vb","vbeg","vbg","vf","vfe","vff","vfm","vgg","vgh","vgl","vgn","vid","vids","vip","vleo","vlog","vn","vnc","vnh","voip","vrsty","vry","vs","vwd","vweg","vzit","vzn",
    "w'sup","w.b.s.","w.e","w.e.","w.o.w","w.o.w.","w/","w/b","w/e","w/end","w/eva","w/o","w/out","w/u","w00t","w012d","w2d","w2f","w2g","w2ho","w2m","w33d","w8","w8am","w8ing","w8t4me","w8ter","w911","wab","wad","wad ^","wadr","wadzup","waf","wafda","wafl","wafm","wafn","wai","waloc","walstib","wam","wamh","wan2tlk","wana","wanafuk","wanker","wanking","wanna","wansta","warez","wassup","wasup","was^","wat","wat's^","watcha","watev","wateva","watevr","watevs","wats","wats ^","wats^","watz ^","wau","waug","wauw","wau^2","waw","waycb","wayd","waygow","wayh","wayn","waysttm","waysw","wayt","wayta","wayut","waz","waz ^","wazz","wazza","wazzup","waz^","wb","wbagnfarb","wbb","wbbs","wbp","wbrb","wbs","wbu","wby","wc","wc3","wcutm","wcw","wd","wdf","wdhlm","wdidn","wdim","wdtm","wduc","wdum","wdus","wdut","wdutom","wduw","wduwta","wduwtta","wdwdn","wdwgw","wdya","wdydt","wdye","wdyl","wdym","wdys","wdyt","wdytia","wdyw","wdywd","wdywtd","wdywtdt","wdywtta","webby","weg","welc","wen","werkz","wev","weve","wevr","wfh","wfhw","wfm","wfyb","wg","wgac","wgaf","wgas","wgasa","wgo","wgph2","wha","whaddya","whaletail","whatcha","whatev","whatevs","whats ^","what^","whenevs","whevah","whever","whf","whit","whodi","whr","whs","wht","whteva","whteve","whtever","whtevr","whtvr","wht^","whubu2","whubut","whut","whyb","whyd","wid","widout","wieu2","wif","wiid","wilco","winnar","wio","wip","wit","witcha","witfp","witu","witw","witwct","witwu","witwwyt","wiu","wiuwu","wiv","wiw","wiwhu","wiwt","wiyp","wjwd","wk","wkd","wkend","wl","wlc","wlcb","wlcm","wld","wlkd","wlos","wltm","wmao","wmd","wmgl","wml","wmyb","wn","wna","wnkr","wnrn","wnt","wntd","woa","woc","wochit","woe","woft","wogge","wogs","wolo","wombat","woot","wot","wotevs","wotv","wotw","woum","wowzers","woz","wp","wpe","wrd","wrdo","wrgad","wrgaf","wrk","wrm","wrng","wrt","wrtg","wrthls","wru","wrud","wruf","wruu2","wsb","wsf","wshtf","wsi","wsibt","wsidi","wsop","wswta","wtb","wtbd","wtbh","wtc","wtcf","wtd","wtf","wtfaud","wtfay","wtfayd","wtfayt","wtfayta","wtfb","wtfbs","wtfc","wtfdik","wtfdum","wtfduw","wtfdyw","wtfe","wtfever","wtfg","wtfh","wtfhb","wtfhwt","wtfigo","wtfigoh","wtfit","wtfits","wtfiu","wtfiup","wtfiuwy","wtfiwwu","wtfiwwy","wtfiyp","wtfm","wtfmf","wtfo","wtfru","wtfrud","wtfrudng","wtfrudoin","wtfruo","wtfruttd","wtfs","wtfuah","wtful","wtfwjd","wtfwt","wtfwtd","wtfwtf","wtfya","wtfyb","wtg","wtgds","wtgp","wth","wtharud","wthau","wthauwf","wthay","wthayd","wthaydwmgf","wthdydt","wthhyb","wthigo","wthiwwu","wtho","wthru","wthrud","wths","wthswm","wthwt","wthwut","wthyi","wtii","wtiiot","wtityb","wtly","wtmf","wtmfh","wtmi","wtmtr","wtp","wtrud","wts","wtt","wttp","wtv","wtva","wtvr","wtwm","wtwr","wu","wu","wu2kilu","wub","wubmgf","wubu2","wubut","wud","wudev","wudn","wugowm","wula","wuld","wuny","wussup","wut","wutb","wutcha","wuteva","wutevr","wuts","wutup","wuu2","wuu2","wuu22m","wuut","wuv","wuwh","wuwt","wuwta","wuwtab","wuwtb","wuwtta","wuwttb","wuwu","wuz","wuza","wuzup","wwc","wwcnd","wwdhd","wwe","wwgf","wwhw","wwikt","wwjd","wwt","wwtf","wwudtm","wwut","www","wwwy","wwy","wwycm","wwyd","wwyd2m","wwyt","wy","wyas","wyatb","wyauimg","wybts","wyc","wycm","wyd","wyg","wygac","wygam","wygowm","wygwm","wyhi","wyhswm","wyltk","wylym","wym","wyn","wyp","wypsu","wys","wysiayg","wysitwirl","wysiwyg","wyw","wywh","wywo","w\\e",
    "x treme","xb36t","xbf","xbl","xcept","xcpt","xd","xellent","xfer","xgf","xing","xit","xl","xlnt","xmas","xmpl","xoac","xor","xover","xox","xoxo","xp","xpect","xplaned","xpt","xroads","xs","xtc","xtra","xtreme","xyz","xyzpdq",
    "y","y w","y!a","y'all","y/n","y/o","y00","y2b","y2k","ya","yaaf","yaafm","yaagf","yaai","yaf","yafi","yag","yall","yapa","yaqw","yarly","yas","yasan","yasf","yasfg","yasg","yasw","yatb","yatwl","yaw","yayo","ybbg","ybs","ybya","ycliu","ycmtsu","ycntu","yctwuw","ydpos","ydtm","ydufc","yduwtk","ye","yea","yer","yermom","yesh","yew","yfb","yfg","yfi","ygg","ygm","ygp","ygpm","ygrr","ygtbfkm","ygtbk","ygtbkm","ygtbsm","ygtsr","yh","yhbt","yhew","yhf","yhgtbsm","yhl","yhm","yhpm","yhtbt","yid","yim","yiwtgo","yk","yki","ykisa","ykm","ykn","ykw","ykwim","ykwya","ykywywm","ylb","ym","ymbkm","yme","ymfp","ymg2c","ymgtc","ymiaw","ymislidi","ymmd","ymmv","ymrasu","yn","yng","ynk","ynm","ynt","ynw","yo","yo'","yodo","yolo","yolt","yomank","yooh","yor","youngin","yoy","ypmf","ypmo","ypom","yqw","yr","yrbk","yrms","yrs","yrsaf","yrsm","yrss","yru","yrubm","yrusm","ys","ysa","ysal","ysati","ysf","ysic","ysitm","ysm","ysoab","yss","yswnt","yt","ytd","ytf","ytfwudt","ythwudt","ytis","ytm","ytmnd","yty","yu","yua","yuo","yup","yur","yust","yvfw","yvw","yw","ywapom","ywia","ywic","yws","ywsyls","ywud","ywvm","ywywm","yysw",
    "z'omg","z0mg","zex","zh","zig","zomfg","zomg","zomgzorrz","zoot","zot","zt","zup")

  val dataDumpDateStr = "2015-08-19"
  val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  val dataDumpDate = LocalDate.parse(dataDumpDateStr, formatter)

  object CodeTypes extends Enumeration {
    type CodeType = Value
    val Java, XML, JSON, StackTrace, Undefined = Value
  }

  val pw_questions = new PrintWriter(filedir + "questions_" + filename)
  pw_questions.write("id," +
    "days since posted," +
    "title length," +
    "view count," +
    "tags count," +
    "max tag popularity (no java)," +
    "avg tag popularity," +
    "min tag popularity," +
    "local max tag popularity (no java)," +
    "local avg tag popularity," +
    "local min tag popularity," +
    "total code %," +
    "java %," +
    "json %," +
    "xml %," +
    "stack traces %," +
    "length," +
    "words count," +
    "text speak count," +
    "urls count," +
    "Coleman-Liau Index," +
    "Flesch Reading Ease Score," +
    "Flesch-Kincaid Grade Level," +
    "Automated Readability Index," +
    "Gunning Fog Index," +
    "SMOG Grade," +
    "day of week," +
    "reputation," +
//    "acceptance rate," + STORMED always returns NONE
    "intercalations," +
    "score," +
    "number of answers," +
    "max answer score," +
    "avg answer score," +
    "min answer score," +
    "number of comments," +
    "max answer length," +
    "avg answer length," +
    "min answer length," +
    "has accepted answer\n")

  val pw_answers = new PrintWriter(filedir + "answers_" + filename)
  pw_answers.write("question_id," +
    "id," +
    "days since posted," +
    "first posted," +
    "same day as question," +
    "question view count," +
    "q tags count," +
    "q max tag popularity (no java)," +
    "q avg tag popularity," +
    "q min tag popularity," +
    "q local max tag popularity (no java)," +
    "q local avg tag popularity," +
    "q local min tag popularity," +
    "total code %," +
    "java %," +
    "json %," +
    "xml %," +
    "stack traces %," +
    "length," +
    "words count," +
    "text speak count," +
    "urls count," +
    "Coleman-Liau Index," +
    "Flesch Reading Ease Score," +
    "Flesch-Kincaid Grade Level," +
    "Automated Readability Index," +
    "Gunning Fog Index," +
    "SMOG Grade," +
    "day of week," +
    "reputation," +
//    "acceptance rate," + STORMED always returns NONE
    "intercalations," +
    "score," +
    "number of comments," +
    "accepted," +
    "max comment length," +
    "avg comment length," +
    "min comment length\n")

  def finish(): Unit = {
    pw_questions.close()
    pw_answers.close()

    System.out.println("Analyzed a total of " + numFiles + " discussions.")
  }

  def processDiscussion(artifact: StackOverflowArtifact): Unit = {

    if(!tagFilters.forall(artifact.question.tags.contains))
      return

    if(exclusiveTags && !artifact.question.tags.forall(tagFilters.contains))
      return

    val daysSincePosted = dataDumpDate.toEpochDay - artifact.question.creationDate.toInstant.atZone(ZoneId.systemDefault()).toLocalDate.toEpochDay

    numFiles += 1
    val mis = artifact.question.metaInformation
    var answersProperties: AnswersProperties = null

    var textReadability: TextReadabilityMetaInformation = null

    for (mi <- artifact.question.metaInformation)
      mi match {
        case m:TextReadabilityMetaInformation => textReadability = m
        case _ =>
    }

    var maxTagPop: Long = 0
    var minTagPop: Long = Long.MaxValue
    var avgTagPop: Double = 0.0

    for (tag <- artifact.question.tags) {
      if (tag != "java")
        maxTagPop = Math.max(maxTagPop, TagBank.getTagPopularity(tag))
      minTagPop = Math.min(minTagPop, TagBank.getTagPopularity(tag))
      avgTagPop += TagBank.getTagPopularity(tag)
    }
    avgTagPop /= artifact.question.tags.length

    var lmaxTagPop: Long = 0
    var lminTagPop: Long = Long.MaxValue
    var lavgTagPop: Double = 0.0

    for (tag <- artifact.question.tags) {
      if (tag != "java")
        lmaxTagPop = Math.max(lmaxTagPop, localTags.getTagPopularity(tag))
      lminTagPop = Math.min(lminTagPop, localTags.getTagPopularity(tag))
      lavgTagPop += localTags.getTagPopularity(tag)
    }
    lavgTagPop /= artifact.question.tags.length

    if (artifact.answers.nonEmpty)
      answersProperties = processAnswers(artifact, artifact.question.viewCount, artifact.question.tags.length, maxTagPop, avgTagPop, minTagPop, lmaxTagPop, lavgTagPop, lminTagPop)

    val iuProperties = processInformationUnits(artifact.question.informationUnits)

    //noinspection ScalaDeprecation
    pw_questions.println(Array(artifact.id.toString,
      daysSincePosted,
      artifact.question.title.length,
      artifact.question.viewCount,
      artifact.question.tags.length,
      maxTagPop,
      avgTagPop,
      minTagPop,
      lmaxTagPop,
      lavgTagPop,
      lminTagPop,
      iuProperties.code_p,
      iuProperties.java_p,
      iuProperties.json_p,
      iuProperties.xml_p,
      iuProperties.stack_traces_p,
      iuProperties.total_length,
      iuProperties.words_count,
      iuProperties.text_speak_count,
      iuProperties.urls_count,
      if (textReadability != null) textReadability.colemanLiauIndex else "NA",
      if (textReadability != null) textReadability.fleshReadingEaseScore else "NA",
      if (textReadability != null) textReadability.fleshKincaidGradeLevel else "NA",
      if (textReadability != null) textReadability.automatedReadingIndex else "NA",
      if (textReadability != null) textReadability.gunningFogIndex else "NA",
      if (textReadability != null) textReadability.smogIndex else "NA",
      artifact.question.creationDate.getDay,
      getOwnerReputation(artifact.question.owner),
//      getOwnerAcceptanceRate(artifact.question.owner), STORMED always returns NONE
      iuProperties.intercalations,
      artifact.question.score,
      artifact.answers.length,
      if (answersProperties != null) answersProperties.max_score else "0",
      if (answersProperties != null) answersProperties.avg_score else "0",
      if (answersProperties != null) answersProperties.min_score else "0",
      artifact.question.comments.length,
      if (answersProperties != null) answersProperties.max_length else "0",
      if (answersProperties != null) answersProperties.avg_length else "0",
      if (answersProperties != null) answersProperties.min_length else "0",
      if (artifact.answers.exists(_.isAccepted)) "1" else "0"
    ).mkString(","))
  }

  def processAnswers(artifact: StackOverflowArtifact, viewCount: Int, tagCount: Int, maxTagPop: Long, avgTagPop: Double, minTagPop: Long, lmaxTagPop: Long, lavgTagPop: Double, lminTagPop: Long): AnswersProperties = {
    val it = artifact.answers.iterator
    var maxAnswerScore = 0
    var avgAnswerScore = 0.0
    var minAnswerScore = Int.MaxValue
    var maxAnswerLength = 0
    var avgAnswerLength = 0.0
    var minAnswerLength = Int.MaxValue

    val answerIds = for (ans <- artifact.answers) yield ans.id

    val firstPostedId = answerIds.min

    while (it.hasNext) {
      val answer = it.next()
      maxAnswerScore = Math.max(answer.score, maxAnswerScore)
      minAnswerScore = Math.min(answer.score, minAnswerScore)
      avgAnswerScore += answer.score

      val iusProperties = processInformationUnits(answer.informationUnits)

      var textReadability: TextReadabilityMetaInformation = null

      for (mi <- answer.metaInformation)
        mi match {
          case m:TextReadabilityMetaInformation => textReadability = m
          case _ =>
        }

      maxAnswerLength = Math.max(iusProperties.total_length, maxAnswerLength)
      minAnswerLength = Math.min(iusProperties.total_length, minAnswerLength)
      avgAnswerLength += iusProperties.total_length

      var commentsProperties: CommentsProperties = null
      if (answer.comments.nonEmpty)
        commentsProperties = processComments(answer.comments)

      val fmt: SimpleDateFormat = new SimpleDateFormat("yyyyMMdd")
      val sameDay = fmt.format(answer.creationDate).equals(fmt.format(artifact.question.creationDate))

      val daysSincePosted = dataDumpDate.toEpochDay - answer.creationDate.toInstant.atZone(ZoneId.systemDefault()).toLocalDate.toEpochDay

      //noinspection ScalaDeprecation
      pw_answers.println(Array(artifact.question.id,
        answer.id,
        daysSincePosted,
        if (answer.id == firstPostedId) 1 else 0,
        if (sameDay) 1 else 0,
        viewCount,
        tagCount,
        maxTagPop,
        avgTagPop,
        minTagPop,
        lmaxTagPop,
        lavgTagPop,
        lminTagPop,
        iusProperties.code_p,
        iusProperties.java_p,
        iusProperties.json_p,
        iusProperties.xml_p,
        iusProperties.stack_traces_p,
        iusProperties.total_length,
        iusProperties.words_count,
        iusProperties.text_speak_count,
        iusProperties.urls_count,
        if (textReadability != null) textReadability.colemanLiauIndex else "NA",
        if (textReadability != null) textReadability.fleshReadingEaseScore else "NA",
        if (textReadability != null) textReadability.fleshKincaidGradeLevel else "NA",
        if (textReadability != null) textReadability.automatedReadingIndex else "NA",
        if (textReadability != null) textReadability.gunningFogIndex else "NA",
        if (textReadability != null) textReadability.smogIndex else "NA",
        answer.creationDate.getDay,
        getOwnerReputation(answer.owner),
//        getOwnerAcceptanceRate(answer.owner), STORMED always returns NONE
        iusProperties.intercalations,
        answer.score,
        answer.comments.length,
        if (answer.isAccepted) 1 else 0,
        if (commentsProperties != null) commentsProperties.max_length else "0",
        if (commentsProperties != null) commentsProperties.avg_length else "0",
        if (commentsProperties != null) commentsProperties.min_length else "0"
      ).mkString(","))
    }
    avgAnswerScore /= artifact.answers.length
    avgAnswerLength /= artifact.answers.length


    new AnswersProperties(maxAnswerScore, avgAnswerScore, minAnswerScore, maxAnswerLength, avgAnswerLength, minAnswerLength)
  }

  def processComments(comments: Seq[StackOverflowComment] ): CommentsProperties = {
    val it = comments.iterator
    var max_length: Int = 0
    var min_length: Int = Int.MaxValue
    var avg_length: Double = 0

    while(it.hasNext) {
      val comment = it.next()
      val iuProps = processInformationUnits(comment.informationUnits)

      max_length = Math.max(iuProps.total_length, max_length)
      min_length = Math.min(iuProps.total_length, min_length)
      avg_length += iuProps.total_length
    }
    avg_length /= comments.length

    new CommentsProperties(max_length, avg_length, min_length)
  }

  def processInformationUnits(informationUnits: Seq[InformationUnit]): InformationUnitsProperties = {
    var code_p: Double = 0
    var java_p: Double = 0
    var json_p: Double = 0
    var xml_p: Double = 0
    var stack_traces_p: Double = 0
    var total_length: Int = 0
    var words_count: Int = 0
    var intercalations: Int = 0
    var text_speak_count: Int = 0
    var urls_count: Int = 0

    val url_regex = "^(https?|ftp|file)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]".r

    val it = informationUnits.iterator

    var lastUnit: InformationUnit = null

    while (it.hasNext) {
      val infUnit = it.next()
      total_length += infUnit.rawText.length

      infUnit match {
        case u: CodeTaggedUnit =>
          if (lastUnit != null && lastUnit.isInstanceOf[NaturalLanguageTaggedUnit])
            intercalations += 1

          code_p += u.rawText.length

          var codeTypesFound: mutable.MutableList[CodeTypes.CodeType] = mutable.MutableList()

          u.astNode.fragments.foreach {
            case _: TextFragmentNode => //println ("Text fragment in code ignored") //needs to be first since it derives from JavaASTNode
            case _: JavaASTNode => codeTypesFound += CodeTypes.Java
            case _: XmlASTNode => codeTypesFound += CodeTypes.XML
            case _: JsonASTNode => codeTypesFound += CodeTypes.JSON
            case _: StackTraceASTNode => codeTypesFound += CodeTypes.StackTrace
            case _: CommentNode => println ("Comment node ignored")
            case n => System.err.println ("Unidentified node found: " + n.toString)
          }

          if (codeTypesFound.toSet[CodeTypes.CodeType].size == 1) //check number of unique types
            codeTypesFound.head match {
              case CodeTypes.Java => java_p += infUnit.rawText.length
              case CodeTypes.XML => xml_p += infUnit.rawText.length
              case CodeTypes.JSON => json_p += infUnit.rawText.length
              case CodeTypes.StackTrace => stack_traces_p += infUnit.rawText.length
            }
          else
            System.err.println("Could not identify code type for fragment or found conflicting types")
        case _: NaturalLanguageTaggedUnit =>
          if (lastUnit != null && lastUnit.isInstanceOf[CodeTaggedUnit])
            intercalations += 1
          val words = infUnit.rawText.split("\\W+")
          words_count += words.length
          text_speak_count += words.count(p => textSpeakDictionary.contains(p.toLowerCase))
          urls_count += url_regex.findAllIn(infUnit.rawText).length
        case _ => throw new Exception("Unexpected Information Unit found")
      }

      lastUnit = infUnit
    }
    code_p /= total_length
    java_p /= total_length
    json_p /= total_length
    xml_p /= total_length
    stack_traces_p /= total_length

    new InformationUnitsProperties(code_p, java_p, json_p, xml_p, stack_traces_p, total_length, words_count, intercalations, text_speak_count, urls_count)
  }

  def getOwnerAcceptanceRate(owner: Option[StackOverflowUser]): String = {
    owner match {
      case Some(o) =>
        o.acceptRate match {
          case Some(ar) => ar.toString
          case None => "NA"
        }
      case None =>
        "NA"
    }
  }

  def getOwnerReputation(owner: Option[StackOverflowUser]): String = {
    owner match {
      case Some(o) =>
        o.reputation.toString
      case None =>
        "0"
    }
  }
}
