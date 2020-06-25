      ******************************************************************
      *    CPCLAIM006.INC
      ******************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  CLAIM006-VALUES.
           02  CLAIM006-T.
             03  CLAIM006-G001.
               05  FILLER  PIC  X(4)   VALUE   "G001" .
               05  FILLER  PIC  X(9)   VALUE   "001000101" .
               05  FILLER  PIC  X(40)  
                   VALUE   "医師の指示通り" .
              03  CLAIM006-G003.
               05  FILLER  PIC  X(4)   VALUE   "G003" .
               05  FILLER  PIC  X(9)   VALUE   "001000113" .
               05  FILLER  PIC  X(40)  
                   VALUE   "就寝前使用" .
              03  CLAIM006-G004.
               05  FILLER  PIC  X(4)   VALUE   "G004" .
               05  FILLER  PIC  X(9)   VALUE   "001000803" .
               05  FILLER  PIC  X(40)  
                   VALUE   "朝使用" .
              03  CLAIM006-G006.
               05  FILLER  PIC  X(4)   VALUE   "G006" .
               05  FILLER  PIC  X(9)   VALUE   "001000808" .
               05  FILLER  PIC  X(40)  
                   VALUE   "朝・夕使用" .
              03  CLAIM006-G008.
               05  FILLER  PIC  X(4)   VALUE   "G008" .
               05  FILLER  PIC  X(9)   VALUE   "001000811" .
               05  FILLER  PIC  X(40)  
                   VALUE   "朝・昼・夕使用" .
              03  CLAIM006-G011.
               05  FILLER  PIC  X(4)   VALUE   "G011" .
               05  FILLER  PIC  X(9)   VALUE   "001000006" .
               05  FILLER  PIC  X(40)  
                   VALUE   "混合" .
              03  CLAIM006-G012.
               05  FILLER  PIC  X(4)   VALUE   "G012" .
               05  FILLER  PIC  X(9)   VALUE   "001000150" .
               05  FILLER  PIC  X(40)  
                   VALUE   "１日１回使用" .
              03  CLAIM006-G013.
               05  FILLER  PIC  X(4)   VALUE   "G013" .
               05  FILLER  PIC  X(9)   VALUE   "001000203" .
               05  FILLER  PIC  X(40)  
                   VALUE   "１日２回使用" .
              03  CLAIM006-G014.
               05  FILLER  PIC  X(4)   VALUE   "G014" .
               05  FILLER  PIC  X(9)   VALUE   "001000304" .
               05  FILLER  PIC  X(40)  
                   VALUE   "１日３回使用" .
              03  CLAIM006-G015.
               05  FILLER  PIC  X(4)   VALUE   "G015" .
               05  FILLER  PIC  X(9)   VALUE   "001000403" .
               05  FILLER  PIC  X(40)  
                   VALUE   "１日４回使用" .
              03  CLAIM006-G016.
               05  FILLER  PIC  X(4)   VALUE   "G016" .
               05  FILLER  PIC  X(9)   VALUE   "001000424" .
               05  FILLER  PIC  X(40)  
                   VALUE   "１日５回使用" .
              03  CLAIM006-G020.
               05  FILLER  PIC  X(4)   VALUE   "G020" .
               05  FILLER  PIC  X(9)   VALUE   "001000004" .
               05  FILLER  PIC  X(40)  
                   VALUE   "隔日使用" .
              03  CLAIM006-J111.
               05  FILLER  PIC  X(4)   VALUE   "J111" .
               05  FILLER  PIC  X(9)   VALUE   "001000106" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分１、朝食後服用" .
              03  CLAIM006-J112.
               05  FILLER  PIC  X(4)   VALUE   "J112" .
               05  FILLER  PIC  X(9)   VALUE   "001000107" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分１、朝食前服用" .
              03  CLAIM006-J113.
               05  FILLER  PIC  X(4)   VALUE   "J113" .
               05  FILLER  PIC  X(9)   VALUE   "001000108" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分１、昼食後服用" .
              03  CLAIM006-J114.
               05  FILLER  PIC  X(4)   VALUE   "J114" .
               05  FILLER  PIC  X(9)   VALUE   "001000109" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分１、昼食前服用" .
              03  CLAIM006-J131.
               05  FILLER  PIC  X(4)   VALUE   "J131" .
               05  FILLER  PIC  X(9)   VALUE   "001000110" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分１、夕食後服用" .
              03  CLAIM006-J132.
               05  FILLER  PIC  X(4)   VALUE   "J132" .
               05  FILLER  PIC  X(9)   VALUE   "001000111" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分１、夕食前服用" .
              03  CLAIM006-J18.
               05  FILLER  PIC  X(4)   VALUE   "J18" .
               05  FILLER  PIC  X(9)   VALUE   "001000113" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分１、就寝前服用" .
              03  CLAIM006-J2.
               05  FILLER  PIC  X(4)   VALUE   "J2" .
               05  FILLER  PIC  X(9)   VALUE   "001000221" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分２、１２時間毎服用" .
              03  CLAIM006-J211.
               05  FILLER  PIC  X(4)   VALUE   "J211" .
               05  FILLER  PIC  X(9)   VALUE   "001000211" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分２、朝・昼食後服用" .
              03  CLAIM006-J212.
               05  FILLER  PIC  X(4)   VALUE   "J212" .
               05  FILLER  PIC  X(9)   VALUE   "001000212" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分２、朝・昼食前服用" .
              03  CLAIM006-J221.
               05  FILLER  PIC  X(4)   VALUE   "J221" .
               05  FILLER  PIC  X(9)   VALUE   "001000206" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分２、朝・夕食後服用" .
              03  CLAIM006-J222.
               05  FILLER  PIC  X(4)   VALUE   "J222" .
               05  FILLER  PIC  X(9)   VALUE   "001000207" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分２、朝・夕食前服用" .
              03  CLAIM006-J231.
               05  FILLER  PIC  X(4)   VALUE   "J231" .
               05  FILLER  PIC  X(9)   VALUE   "001000215" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分２、朝食後・就寝前服用" .
              03  CLAIM006-J232.
               05  FILLER  PIC  X(4)   VALUE   "J232" .
               05  FILLER  PIC  X(9)   VALUE   "001000216" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分２、朝食前・就寝前服用" .
              03  CLAIM006-J234.
               05  FILLER  PIC  X(4)   VALUE   "J234" .
               05  FILLER  PIC  X(9)   VALUE   "001000219" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分２、夕食後・就寝前" .
              03  CLAIM006-J3.
               05  FILLER  PIC  X(4)   VALUE   "J3" .
               05  FILLER  PIC  X(9)   VALUE   "001000319" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分３、８時間毎服用" .
              03  CLAIM006-J311.
               05  FILLER  PIC  X(4)   VALUE   "J311" .
               05  FILLER  PIC  X(9)   VALUE   "001000301" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分３、毎食後服用" .
              03  CLAIM006-J312.
               05  FILLER  PIC  X(4)   VALUE   "J312" .
               05  FILLER  PIC  X(9)   VALUE   "001000305" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分３、毎食前服用" .
              03  CLAIM006-J314.
               05  FILLER  PIC  X(4)   VALUE   "J314" .
               05  FILLER  PIC  X(9)   VALUE   "001000309" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分３、朝食後・夕食後・就寝前服用" .
              03  CLAIM006-J33.
               05  FILLER  PIC  X(4)   VALUE   "J33" .
               05  FILLER  PIC  X(9)   VALUE   "001000307" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分３、毎食間服用" .
              03  CLAIM006-J4.
               05  FILLER  PIC  X(4)   VALUE   "J4" .
               05  FILLER  PIC  X(9)   VALUE   "001000408" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分４、６時間毎服用" .
              03  CLAIM006-J412.
               05  FILLER  PIC  X(4)   VALUE   "J412" .
               05  FILLER  PIC  X(9)   VALUE   "001000404" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分４、毎食前・就寝前服用" .
              03  CLAIM006-J413.
               05  FILLER  PIC  X(4)   VALUE   "J413" .
               05  FILLER  PIC  X(9)   VALUE   "001000406" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分４、毎食後・就寝前服用" .
              03  CLAIM006-J5.
               05  FILLER  PIC  X(4)   VALUE   "J5" .
               05  FILLER  PIC  X(9)   VALUE   "001000423" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分５、毎食前・午後３時・就寝前服用" .
              03  CLAIM006-J500.
               05  FILLER  PIC  X(4)   VALUE   "J500" .
               05  FILLER  PIC  X(9)   VALUE   "001000803" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分１、朝" .
              03  CLAIM006-J502.
               05  FILLER  PIC  X(4)   VALUE   "J502" .
               05  FILLER  PIC  X(9)   VALUE   "001000808" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分２、朝・夕" .
              03  CLAIM006-J504.
               05  FILLER  PIC  X(4)   VALUE   "J504" .
               05  FILLER  PIC  X(9)   VALUE   "001000811" .
               05  FILLER  PIC  X(40)  
                   VALUE   "分３、朝・昼・夕" .
              03  CLAIM006-T18.
               05  FILLER  PIC  X(4)   VALUE   "T18" .
               05  FILLER  PIC  X(9)   VALUE   "001000133" .
               05  FILLER  PIC  X(40)  
                   VALUE   "発熱時服用" .
              03  CLAIM006-T20.
               05  FILLER  PIC  X(4)   VALUE   "T20" .
               05  FILLER  PIC  X(9)   VALUE   "001000125" .
               05  FILLER  PIC  X(40)  
                   VALUE   "発作時服用" .
              03  CLAIM006-T21.
               05  FILLER  PIC  X(4)   VALUE   "T21" .
               05  FILLER  PIC  X(9)   VALUE   "001000132" .
               05  FILLER  PIC  X(40)  
                   VALUE   "嘔吐時服用" .
              03  CLAIM006-T22.
               05  FILLER  PIC  X(4)   VALUE   "T22" .
               05  FILLER  PIC  X(9)   VALUE   "001000130" .
               05  FILLER  PIC  X(40)  
                   VALUE   "便秘時服用" .
              03  CLAIM006-T23.
               05  FILLER  PIC  X(4)   VALUE   "T23" .
               05  FILLER  PIC  X(9)   VALUE   "001000114" .
               05  FILLER  PIC  X(40)  
                   VALUE   "不眠時服用" .
              03  CLAIM006-T24.
               05  FILLER  PIC  X(4)   VALUE   "T24" .
               05  FILLER  PIC  X(9)   VALUE   "001000124" .
               05  FILLER  PIC  X(40)  
                   VALUE   "腹痛時服用" .
              03  CLAIM006-T25.
               05  FILLER  PIC  X(4)   VALUE   "T25" .
               05  FILLER  PIC  X(9)   VALUE   "001000123" .
               05  FILLER  PIC  X(40)  
                   VALUE   "頭痛時服用" .
              03  CLAIM006-T26.
               05  FILLER  PIC  X(4)   VALUE   "T26" .
               05  FILLER  PIC  X(9)   VALUE   "001000115" .
               05  FILLER  PIC  X(40)  
                   VALUE   "不安時服用" .
              03  CLAIM006-T27.
               05  FILLER  PIC  X(4)   VALUE   "T27" .
               05  FILLER  PIC  X(9)   VALUE   "001000131" .
               05  FILLER  PIC  X(40)  
                   VALUE   "下痢時服用" .
              03  CLAIM006-T28.
               05  FILLER  PIC  X(4)   VALUE   "T28" .
               05  FILLER  PIC  X(9)   VALUE   "001000125" .
               05  FILLER  PIC  X(40)  
                   VALUE   "咳がひどい時服用" .
              03  CLAIM006-T29.
               05  FILLER  PIC  X(4)   VALUE   "T29" .
               05  FILLER  PIC  X(9)   VALUE   "001000106" .
               05  FILLER  PIC  X(40)  
                   VALUE   "（頓服）分１、朝食後服用" .
              03  CLAIM006-T30.
               05  FILLER  PIC  X(4)   VALUE   "T30" .
               05  FILLER  PIC  X(9)   VALUE   "001000206" .
               05  FILLER  PIC  X(40)  
                   VALUE   "（頓服）分２、朝・夕食後服用" .
              03  CLAIM006-T34.
               05  FILLER  PIC  X(4)   VALUE   "T34" .
               05  FILLER  PIC  X(9)   VALUE   "001000121" .
               05  FILLER  PIC  X(40)  
                   VALUE   "いらいらする時服用" .
              03  CLAIM006-T37.
               05  FILLER  PIC  X(4)   VALUE   "T37" .
               05  FILLER  PIC  X(9)   VALUE   "001000675" .
               05  FILLER  PIC  X(40)  
                   VALUE   "かゆい時に服用" .
              03  CLAIM006-T38.
               05  FILLER  PIC  X(4)   VALUE   "T38" .
               05  FILLER  PIC  X(9)   VALUE   "001000132" .
               05  FILLER  PIC  X(40)  
                   VALUE   "嘔気時服用" .
              03  CLAIM006-T40.
               05  FILLER  PIC  X(4)   VALUE   "T40" .
               05  FILLER  PIC  X(9)   VALUE   "001000114" .
               05  FILLER  PIC  X(40)  
                   VALUE   "夜半不眠時服用" .
           02  CLAIM006-R  REDEFINES   CLAIM006-T.
             03  CLAIM006-TX           OCCURS  55.
               05  CLAIM006-CLAIM       PIC  X(4).
               05  CLAIM006-ORCA        PIC  X(9).
               05  CLAIM006-NAME       PIC  X(40).
