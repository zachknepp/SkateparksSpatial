##Toy model on Texas
library(tidyverse)
library(sf)           # Objects and functions for geospatial data
library(rgdal)        # Functions for spatial data input/output
library(ggplot2)      # Graphing functions
library(dplyr)        # Functions for processing tabular data
library(tidyr)        # Functions for processing tabular data
library(scales)       # Additional graphics functions
library(RColorBrewer) # Color ramps for graphs and maps
library(gridExtra)    # Functions for arranging multiple plots on a page
library(readr)
#import parks
txparks <- read.csv("C:/Users/zachk/Desktop/Econ Senior Project/Texas/texasParks (1).csv", header = TRUE)
colnames(txparks)[6]<-"zipcode"
#import zip fips
txzipfips <- read.csv("C:/Users/zachk/Desktop/Econ Senior Project/Texas/txzipfips.csv", header = TRUE)
colnames(txzipfips)[1]<-"zipcode"
  #only need the first 2 columns

#import texas master
txcensus <- read.csv("C:/Users/zachk/Desktop/Econ Senior Project/Texas/texascensusmaster.csv", header = TRUE)
colnames(txcensus)[1] <- "FIPS"

#use zip fips to add county code to each park (mutate)
txparksfip <- merge(x = txparks, y = txzipfips, by = "zipcode")
colnames(txparksfip)[15] <- "FIPS"
#omit crappy parks

txparksfip1 <-txparksfip %>%
  group_by(FIPS) %>%
  summarize(no_obs =n())
#Add county info to frame
txskatecensus = merge(x = txcensus, y = txparksfip1, by = "FIPS", all.x=TRUE)
txskatecensus$skateparks <- replace_na(txskatecensus$no_obs, 0)
write.csv(txskatecensus,"C:/Users/zachk/Desktop/Econ Senior Project/Texas/txMaster.csv", row.names = FALSE)
#County statistics:

#Add CDC death rates
#txdeaths <- read.csv("C:/Users/zachk/Desktop/Econ Senior Project/Texas/Texas Death Rates.csv", header = TRUE)
#Reading CDC's death table as text, call it de.
##########################
de = read.table(text = '
   Name             FIPS	Deaths	Population	CrudeRate
	"Anderson County, TX"	"48001"	13819	1256976	1099.4
	"Andrews County, TX"	"48003"	2705	336436	804.0
	"Angelina County, TX"	"48005"	18725	1867950	1002.4
	"Aransas County, TX"	"48007"	6995	519808	1345.7
	"Archer County, TX"	"48009"	1718	194894	881.5
	"Armstrong County, TX"	"48011"	619	43288	1430.0
	"Atascosa County, TX"	"48013"	8086	989312	817.3
	"Austin County, TX"	"48015"	6061	606805	998.8
	"Bailey County, TX"	"48017"	1352	152921	884.1
	"Bandera County, TX"	"48019"	4391	448206	979.7
	"Bastrop County, TX"	"48021"	12664	1619517	782.0
	"Baylor County, TX"	"48023"	1399	82854	1688.5
	"Bee County, TX"	"48025"	5352	710723	753.0
	"Bell County, TX"	"48027"	42455	6619482	641.4
	"Bexar County, TX"	"48029"	258866	37395231	692.2
	"Blanco County, TX"	"48031"	2407	225565	1067.1
	"Borden County, TX"	"48033"	105	14422	728.1
	"Bosque County, TX"	"48035"	5630	395196	1424.6
	"Bowie County, TX"	"48037"	22026	2015139	1093.0
	"Brazoria County, TX"	"48039"	44444	6788887	654.7
	"Brazos County, TX"	"48041"	19636	4200418	467.5
	"Brewster County, TX"	"48043"	1716	199683	859.4
	"Briscoe County, TX"	"48045"	434	35973	1206.5
	"Brooks County, TX"	"48047"	1783	162609	1096.5
	"Brown County, TX"	"48049"	10768	834290	1290.7
	"Burleson County, TX"	"48051"	4214	379769	1109.6
	"Burnet County, TX"	"48053"	9690	926459	1045.9
	"Caldwell County, TX"	"48055"	6943	838100	828.4
	"Calhoun County, TX"	"48057"	4298	466308	921.7
	"Callahan County, TX"	"48059"	3576	295277	1211.1
	"Cameron County, TX"	"48061"	52906	8628899	613.1
	"Camp County, TX"	"48063"	3246	270058	1202.0
	"Carson County, TX"	"48065"	1445	136891	1055.6
	"Cass County, TX"	"48067"	8900	664301	1339.8
	"Castro County, TX"	"48069"	1393	173899	801.0
	"Chambers County, TX"	"48071"	5302	752433	704.6
	"Cherokee County, TX"	"48073"	11757	1099831	1069.0
	"Childress County, TX"	"48075"	1754	158996	1103.2
	"Clay County, TX"	"48077"	2552	237152	1076.1
	"Cochran County, TX"	"48079"	735	70009	1049.9
	"Coke County, TX"	"48081"	1202	75781	1586.1
	"Coleman County, TX"	"48083"	3105	191580	1620.7
	"Collin County, TX"	"48085"	70391	16922693	416.0
	"Collingsworth County, TX"	"48087"	953	66723	1428.3
	"Colorado County, TX"	"48089"	5728	456804	1253.9
	"Comal County, TX"	"48091"	21058	2437372	864.0
	"Comanche County, TX"	"48093"	4083	301811	1352.8
	"Concho County, TX"	"48095"	734	85124	862.3
	"Cooke County, TX"	"48097"	8838	847528	1042.8
	"Coryell County, TX"	"48099"	9085	1640622	553.8
	"Cottle County, TX"	"48101"	538	34249	1570.8
	"Crane County, TX"	"48103"	867	95795	905.1
	"Crockett County, TX"	"48105"	848	82883	1023.1
	"Crosby County, TX"	"48107"	1654	137944	1199.0
	"Culberson County, TX"	"48109"	435	54653	795.9
	"Dallam County, TX"	"48111"	1106	146952	752.6
	"Dallas County, TX"	"48113"	328557	52777962	622.5
	"Dawson County, TX"	"48115"	3111	303425	1025.3
	"Deaf Smith County, TX"	"48117"	3390	414621	817.6
	"Delta County, TX"	"48119"	1583	116265	1361.5
	"Denton County, TX"	"48121"	61460	14494566	424.0
	"DeWitt County, TX"	"48123"	5687	445110	1277.7
	"Dickens County, TX"	"48125"	685	53405	1282.7
	"Dimmit County, TX"	"48127"	1963	225274	871.4
	"Donley County, TX"	"48129"	1145	79993	1431.4
	"Duval County, TX"	"48131"	2743	263595	1040.6
	"Eastland County, TX"	"48133"	5896	404297	1458.3
	"Ector County, TX"	"48135"	25382	3081664	823.6
	"Edwards County, TX"	"48137"	451	44087	1023.0
	"Ellis County, TX"	"48139"	23468	3232313	726.0
	"El Paso County, TX"	"48141"	106896	17093880	625.3
	"Erath County, TX"	"48143"	7124	832983	855.2
	"Falls County, TX"	"48145"	4515	390438	1156.4
	"Fannin County, TX"	"48147"	9459	736271	1284.7
	"Fayette County, TX"	"48149"	7000	529705	1321.5
	"Fisher County, TX"	"48151"	1201	88320	1359.8
	"Floyd County, TX"	"48153"	1635	144707	1129.9
	"Foard County, TX"	"48155"	447	29982	1490.9
	"Fort Bend County, TX"	"48157"	53111	12732616	417.1
	"Franklin County, TX"	"48159"	2550	228161	1117.6
	"Freestone County, TX"	"48161"	4752	422396	1125.0
	"Frio County, TX"	"48163"	2876	389290	738.8
	"Gaines County, TX"	"48165"	2454	386498	634.9
	"Galveston County, TX"	"48167"	54213	6482856	836.3
	"Garza County, TX"	"48169"	1139	132615	858.9
	"Gillespie County, TX"	"48171"	7105	533358	1332.1
	"Glasscock County, TX"	"48173"	129	28699	449.5
	"Goliad County, TX"	"48175"	1652	159946	1032.8
	"Gonzales County, TX"	"48177"	4577	436692	1048.1
	"Gray County, TX"	"48179"	6011	493319	1218.5
	"Grayson County, TX"	"48181"	29772	2668311	1115.8
	"Gregg County, TX"	"48183"	27747	2624009	1057.4
	"Grimes County, TX"	"48185"	5758	580243	992.3
	"Guadalupe County, TX"	"48187"	19375	2814538	688.4
	"Hale County, TX"	"48189"	6861	778809	881.0
	"Hall County, TX"	"48191"	1142	74459	1533.7
	"Hamilton County, TX"	"48193"	2960	183315	1614.7
	"Hansford County, TX"	"48195"	1205	119232	1010.6
	"Hardeman County, TX"	"48197"	1128	92953	1213.5
	"Hardin County, TX"	"48199"	11768	1174176	1002.2
	"Harris County, TX"	"48201"	509899	89547003	569.4
	"Harrison County, TX"	"48203"	14020	1425675	983.4
	"Hartley County, TX"	"48205"	947	127745	741.3
	"Haskell County, TX"	"48207"	1891	129020	1465.7
	"Hays County, TX"	"48209"	17784	3500672	508.0
	"Hemphill County, TX"	"48211"	738	82310	896.6
	"Henderson County, TX"	"48213"	21798	1722589	1265.4
	"Hidalgo County, TX"	"48215"	83237	16380793	508.1
	"Hill County, TX"	"48217"	9345	761989	1226.4
	"Hockley County, TX"	"48219"	4672	506532	922.4
	"Hood County, TX"	"48221"	13232	1118924	1182.6
	"Hopkins County, TX"	"48223"	8412	762290	1103.5
	"Houston County, TX"	"48225"	6741	511111	1318.9
	"Howard County, TX"	"48227"	8323	768775	1082.6
	"Hudspeth County, TX"	"48229"	446	80486	554.1
	"Hunt County, TX"	"48231"	18746	1900011	986.6
	"Hutchinson County, TX"	"48233"	5601	487285	1149.4
	"Irion County, TX"	"48235"	286	35447	806.8
	"Jack County, TX"	"48237"	2011	196612	1022.8
	"Jackson County, TX"	"48239"	3411	315664	1080.6
	"Jasper County, TX"	"48241"	9125	782019	1166.9
	"Jeff Davis County, TX"	"48243"	434	49495	876.9
	"Jefferson County, TX"	"48245"	55629	5534105	1005.2
	"Jim Hogg County, TX"	"48247"	1109	114720	966.7
	"Jim Wells County, TX"	"48249"	8326	893073	932.3
	"Johnson County, TX"	"48251"	27126	3303475	821.1
	"Jones County, TX"	"48253"	4390	443311	990.3
	"Karnes County, TX"	"48255"	3364	333403	1009.0
	"Kaufman County, TX"	"48257"	18446	2233432	825.9
	"Kendall County, TX"	"48259"	6777	749720	903.9
	"Kenedy County, TX"	"48261"	76	9204	825.7
	"Kent County, TX"	"48263"	303	17504	1731.0
	"Kerr County, TX"	"48265"	15076	1070452	1408.4
	"Kimble County, TX"	"48267"	1317	98849	1332.3
	"King County, TX"	"48269"	34	6422	529.4
	"Kinney County, TX"	"48271"	817	77934	1048.3
	"Kleberg County, TX"	"48273"	5526	693022	797.4
	"Knox County, TX"	"48275"	1277	84468	1511.8
	"Lamar County, TX"	"48277"	13397	1086373	1233.2
	"Lamb County, TX"	"48279"	3652	307091	1189.2
	"Lampasas County, TX"	"48281"	4476	434224	1030.8
	"La Salle County, TX"	"48283"	1157	150626	768.1
	"Lavaca County, TX"	"48285"	5666	428292	1322.9
	"Lee County, TX"	"48287"	3583	364065	984.2
	"Leon County, TX"	"48289"	4629	363604	1273.1
	"Liberty County, TX"	"48291"	16161	1699385	951.0
	"Limestone County, TX"	"48293"	6237	507641	1228.6
	"Lipscomb County, TX"	"48295"	709	71399	993.0
	"Live Oak County, TX"	"48297"	2463	261346	942.4
	"Llano County, TX"	"48299"	6462	422657	1528.9
	"Loving County, TX"	"48301"	15	2063	Unreliable
	"Lubbock County, TX"	"48303"	50111	6088055	823.1
	"Lynn County, TX"	"48305"	1359	132889	1022.7
	"McCulloch County, TX"	"48307"	2507	179299	1398.2
	"McLennan County, TX"	"48309"	45755	5144458	889.4
	"McMullen County, TX"	"48311"	177	16902	1047.2
	"Madison County, TX"	"48313"	2991	299021	1000.3
	"Marion County, TX"	"48315"	3464	232276	1491.3
	"Martin County, TX"	"48317"	1037	110371	939.6
	"Mason County, TX"	"48319"	1090	87380	1247.4
	"Matagorda County, TX"	"48321"	8193	813900	1006.6
	"Maverick County, TX"	"48323"	7557	1174251	643.6
	"Medina County, TX"	"48325"	8341	1001959	832.5
	"Menard County, TX"	"48327"	771	48829	1579.0
	"Midland County, TX"	"48329"	22137	3100578	714.0
	"Milam County, TX"	"48331"	6508	543951	1196.4
	"Mills County, TX"	"48333"	1553	108175	1435.6
	"Mitchell County, TX"	"48335"	2245	201576	1113.7
	"Montague County, TX"	"48337"	6276	429653	1460.7
	"Montgomery County, TX"	"48339"	64954	9887130	657.0
	"Moore County, TX"	"48341"	3171	465139	681.7
	"Morris County, TX"	"48343"	3897	282858	1377.7
	"Motley County, TX"	"48345"	416	27324	1522.5
	"Nacogdoches County, TX"	"48347"	12513	1388720	901.0
	"Navarro County, TX"	"48349"	11499	1046077	1099.2
	"Newton County, TX"	"48351"	3568	315800	1129.8
	"Nolan County, TX"	"48353"	4171	331723	1257.4
	"Nueces County, TX"	"48355"	59760	7464037	800.6
	"Ochiltree County, TX"	"48357"	1727	216895	796.2
	"Oldham County, TX"	"48359"	353	45700	772.4
	"Orange County, TX"	"48361"	20442	1834351	1114.4
	"Palo Pinto County, TX"	"48363"	7323	613593	1193.5
	"Panola County, TX"	"48365"	5986	513865	1164.9
	"Parker County, TX"	"48367"	20759	2520084	823.7
	"Parmer County, TX"	"48369"	1768	219549	805.3
	"Pecos County, TX"	"48371"	2698	347750	775.8
	"Polk County, TX"	"48373"	13695	1013878	1350.8
	"Potter County, TX"	"48375"	26782	2614034	1024.5
	"Presidio County, TX"	"48377"	987	161489	611.2
	"Rains County, TX"	"48379"	2824	239324	1180.0
	"Randall County, TX"	"48381"	20224	2644844	764.7
	"Reagan County, TX"	"48383"	580	75869	764.5
	"Real County, TX"	"48385"	1046	71861	1455.6
	"Red River County, TX"	"48387"	4461	285769	1561.1
	"Reeves County, TX"	"48389"	2453	305590	802.7
	"Refugio County, TX"	"48391"	2030	162919	1246.0
	"Roberts County, TX"	"48393"	211	19322	1092.0
	"Robertson County, TX"	"48395"	4322	364269	1186.5
	"Rockwall County, TX"	"48397"	10023	1651282	607.0
	"Runnels County, TX"	"48399"	3181	233863	1360.2
	"Rusk County, TX"	"48401"	11923	1135269	1050.2
	"Sabine County, TX"	"48403"	3722	231214	1609.8
	"San Augustine County, TX"	"48405"	2995	192694	1554.3
	"San Jacinto County, TX"	"48407"	6032	571477	1055.5
	"San Patricio County, TX"	"48409"	12869	1465509	878.1
	"San Saba County, TX"	"48411"	1625	132271	1228.5
	"Schleicher County, TX"	"48413"	661	67959	972.6
	"Scurry County, TX"	"48415"	3765	366853	1026.3
	"Shackelford County, TX"	"48417"	832	73168	1137.1
	"Shelby County, TX"	"48419"	6681	558726	1195.8
	"Sherman County, TX"	"48421"	591	67407	876.8
	"Smith County, TX"	"48423"	42009	4523343	928.7
	"Somervell County, TX"	"48425"	1815	179347	1012.0
	"Starr County, TX"	"48427"	8006	1323111	605.1
	"Stephens County, TX"	"48429"	2557	208685	1225.3
	"Sterling County, TX"	"48431"	282	27822	1013.6
	"Stonewall County, TX"	"48433"	518	32036	1616.9
	"Sutton County, TX"	"48435"	761	88835	856.6
	"Swisher County, TX"	"48437"	1843	171892	1072.2
	"Tarrant County, TX"	"48439"	248348	39288156	632.1
	"Taylor County, TX"	"48441"	27909	2890457	965.6
	"Terrell County, TX"	"48443"	247	20191	1223.3
	"Terry County, TX"	"48445"	2848	276558	1029.8
	"Throckmorton County, TX"	"48447"	490	36168	1354.8
	"Titus County, TX"	"48449"	6006	681728	881.0
	"Tom Green County, TX"	"48451"	22841	2438046	936.9
	"Travis County, TX"	"48453"	105668	22660747	466.3
	"Trinity County, TX"	"48455"	4855	315665	1538.0
	"Tyler County, TX"	"48457"	5771	468482	1231.9
	"Upshur County, TX"	"48459"	9700	857114	1131.7
	"Upton County, TX"	"48461"	709	74459	952.2
	"Uvalde County, TX"	"48463"	5325	583579	912.5
	"Val Verde County, TX"	"48465"	7691	1050138	732.4
	"Van Zandt County, TX"	"48467"	13866	1153038	1202.6
	"Victoria County, TX"	"48469"	16961	1930871	878.4
	"Walker County, TX"	"48471"	10598	1480317	715.9
	"Waller County, TX"	"48473"	6334	943882	671.1
	"Ward County, TX"	"48475"	2452	241185	1016.6
	"Washington County, TX"	"48477"	8065	728483	1107.1
	"Webb County, TX"	"48479"	26571	5333013	498.2
	"Wharton County, TX"	"48481"	9194	907291	1013.3
	"Wheeler County, TX"	"48483"	1589	116218	1367.3
	"Wichita County, TX"	"48485"	29307	2892710	1013.1
	"Wilbarger County, TX"	"48487"	3784	297449	1272.2
	"Willacy County, TX"	"48489"	3262	468798	695.8
	"Williamson County, TX"	"48491"	44451	9179013	484.3
	"Wilson County, TX"	"48493"	7409	926303	799.8
	"Winkler County, TX"	"48495"	1626	160332	1014.1
	"Wise County, TX"	"48497"	11213	1298929	863.2
	"Wood County, TX"	"48499"	12762	912558	1398.5
	"Yoakum County, TX"	"48501"	1324	173991	761.0
	"Young County, TX"	"48503"	5720	398210	1436.4
	"Zapata County, TX"	"48505"	1954	298205	655.3
	"Zavala County, TX"	"48507"	2138	259339	824.4', header=TRUE)
##################
#Makin our death census
deadlycensus = merge(x = txskatecensus, y = de, by = "FIPS")
write.csv(deadlycensus,"C:/Users/zachk/Desktop/Econ Senior Project/Texas/TXtoMap.csv", row.names = FALSE)

#Maps and stuff
  #First, we need a shapefile

