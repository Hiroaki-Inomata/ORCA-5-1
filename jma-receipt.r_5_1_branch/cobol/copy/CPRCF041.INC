      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *    �쥻�ץ����ٽ�     COPY (CPRCF041.INC)                *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.01.01   NACL-����     03.02.06  �����ȥ�����ȥե饰���ɲ�
      *  01.01.02   NACL-����     03.06.19  �������ơ��֥���ɲ�
      *  01.01.03   NACL-����     04.07.28  �ٶȾ����ե饰���ٶȴ��ֽ�λ
      *                                     �������ɲ�
      *  01.01.04   NACL-����     05.01.07  ������ô���女���ɡ���������
      *                                     �����̾���ɲ�
      *  01.01.05   NACL-����     05.01.26  ��������������ɲ�
      *  01.01.06   NACL-����     05.08.02  �����ս����ͼ��ѹ����ɲ�
      *  01.01.07   NACL-����     05.12.01  ϫ�ҥ쥻������ܡ����ǯ��
      *                                     ϫ�ҥ쥻������ܡ����
      *                                     ϫ�ҥ쥻�ץȲ��
      *                                     ϫ�ҥ쥻������ܤ��ɲ�
      *
      *  02.09.01    NACL-ƣ��    06/04/14  ��������ǯ��������б�
      *                                     �����®���θ����û��ե饰�ɲ�
      *  03.04.01    NACL-����    06/12/01  �����շ��ݽ���б�
      *  03.05.01    NACL-����    07/05/28  ���롼�׿����б�
      *
      *  04.01.01    NACL-����    07/10/25  ��̳�ҳ��б�
      *  04.02.01    NACL-����    08/02/26  �����ʬ�ɲ�
      *
      *  04.05.01    NACL-����    09/06/29  �ٶȴ��ֳ��Ϸ�����
      *                                     �ٶȾ������ܴ��ֵ��ܤ��ɲ�
      *  04.05.02    NACL-����    10/02/16  ͽ�������ɲ�
      *
      *  04.06.01    NACL-����    10/07/02  ����������������󥻥����
      *                                     �ʣ����ܡ˻���ե饰�ɲ�
      *
      *  04.07.01    NACL-����    11/09/01  ϫ�ҽ������ʬ�ե饰�ɲ�
      *                                     ϫ�Ҵ����Ľ𥳡����ɲ�
      *                                     ǯ���ɲ�
      *                                     �ƥ��ȴ����ֹ��ʬ�ɲ�
      *  04.07.02    NACL-����    13/01/21  ϫ�ҥ쥻���б�
      *  04.07.03    NACL-����    13/04/16  ϫ�ҥ쥻���б�
      *                                     ϫ�һ�����ŵ��ض�ʬ�ɲ�
      *  04.07.04    NACL-����    13/08/20  �����������ǻϤޤ륳���ɤ�
      *                                     ̵ͭ��ʬ�ɲ�
      *  04.07.05    NACL-ƣ��    13/09/20  �軰�Թ԰��б�
      *  04.07.06    NACL-����    14/07/31  �軰�Թ԰٤˴ؤ���ե饰�ɲ�
      *  04.07.07    NACL-����    14/10/15  �����ե饰�ɲ�
      *  04.07.08    NACL-����    15/09/08  �����աʿ��ͼ��ˡʣϣã��ͼ���
      *                                     �Ѥι����ɲ�
      *
      *  04.08.01    NACL-����    15/08/27  �����ա���������ü������
      *                                     ��ʬ�ɲ�
      *  04.08.02    NACL-����    17/02/24  �쥻�ץȼ����������顼�����ɲ�
      *  04.08.03    NACL-����    18/04/06  ʿ������ǯ��������б�
      *  04.08.04    NACL-����    18/05/22  ʿ������ǯ��������б�
      *  04.08.05    NACL-����    18/06/11  �����������פΥ������б�
      *
      *  05.00.01    NACL-����    19/07/01  ��ϥӥ�ȯ�����쥻�ץȵ����б�
      *                                     �ʵ�ǽ�ɲá�
      *  05.00.02    NACL-����    20/04/07  ���£�ǯ��������б�
      ************************************************************
      *01  RECE41-REC.
           03  RECE41-KEY.
      *���ŵ��أɣ�
               05  RECE41-HOSPNUM         PIC  9(02).
      *����ǯ��
               05  RECE41-SRYYM           PIC  9(06).
      *������ʬ
               05  RECE41-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  RECE41-PTID            PIC  9(10).
      *�쥻�ץȲ�
               05  RECE41-RECEKA          PIC  X(02).
      *�ݸ��ɣ�        
               05  RECE41-HKNID           PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  RECE41-TEKSTYMD        PIC  X(08). 
      *�쥻�ץȼ���
           03  RECE41-RECESYUBETU         PIC  9(04).
      *�����ֹ�
           03  RECE41-PTNUM               PIC  X(20).
      *���Ųʾ���
           03  RECE41-SRYKAINF            OCCURS  20.
      *���Ų�
               05  RECE41-SRYKA           PIC  X(02).
      *���ʻ�̾
           03  RECE41-KANANAME            PIC  X(50).
      *������̾
           03  RECE41-NAME                PIC  X(100).
      *����
           03  RECE41-SEX                 PIC  X(01).
      *��ǯ����
           03  RECE41-BIRTHDAY            PIC  9(08).
      *��˴��ʬ
           03  RECE41-DEATHKBN            PIC  X(01).
      *ǯ��
           03  RECE41-AGE                 PIC  9(03).
      *�ƥ��ȴ����ֹ��ʬ
           03  RECE41-TSTPTNUMKBN         PIC  X(01).
      *
      *���ݸ�����
           03  RECE41-HKN.
      *���ݸ���ϫ���ݸ���ʬ
               05  RECE41-HKNKBN          PIC  X(01).
      *���ݸ����ݸ��ֹ�
               05  RECE41-HKNNUM          PIC  X(03).
      *���ݸ��ݴ��Ը����ֹ�
               05  RECE41-KOFUNUM         PIC  X(14).
      *���ݸ��ݻ��Ƚ�̾��
               05  RECE41-JIGYOUNAME      PIC  X(80).
      *���ݸ��ݽ������ƻ�ܸ�̾
               05  RECE41-PREFNAME        PIC  X(20).
      *���ݸ��ݽ������ƻ�ܸ���ʬ
               05  RECE41-PREFKBN         PIC  X(01).
      *���ݸ��ݽ���Ϸ��Զ�̾
               05  RECE41-CITYNAME        PIC  X(80).
      *���ݸ��ݽ���Ϸ��Զ��ʬ
               05  RECE41-CITYKBN         PIC  X(01).
      *���ݸ��ݿ��Ѻ���
               05  RECE41-SINKEI          PIC  X(01).
      *���ݸ���ž����ͳ
               05  RECE41-TENKI           PIC  X(01).
      *���ݸ��ݽ���ǯ����
               05  RECE41-SHOBYOYMD       PIC  X(08).
      *���ݸ��ݻͻ������ʬ
               05  RECE41-SISIKBN         PIC  X(01).
      *���ݸ������ܳ���ǯ����
               05  RECE41-RYOSTYMD        PIC  X(08).
      *���ݸ������ܽ�λǯ����
               05  RECE41-RYOEDYMD        PIC  X(08).
      *���ݸ���»����ʬ
               05  RECE41-SONSHOUKBN      PIC  X(02).
      *���ݸ���»����ʬ�ݻ���
               05  RECE41-SONSHOUKBN-EDANUM  PIC  X(01).
      *���ݸ��ݿǻ�ǯ����
               05  RECE41-SINSATUYMD      PIC  X(08).
      *���ݸ���ϫ�ҥ�����
               05  RECE41-COMMENT         PIC  X(200).
      *���ݸ��ݺҳ���ʬ
               05  RECE41-SAIGAIKBN       PIC  X(01).
      *���ݸ��ݼ�����ô���女����
               05  RECE41-DRCD            PIC  X(05).
      *���ݸ��ݼ������ݸ����̾
               05  RECE41-HOKENNAME       PIC  X(80).
      *���ݸ���ϫ�ҥ쥻������ܡ����ǯ��
               05  RECE41-KIJYUNYM-KSI    PIC  X(06).
      *���ݸ���ϫ�ҥ쥻������ܡ����
               05  RECE41-KAISU-KSI       PIC  9(03).
      *���ݸ��������ʬ
               05  RECE41-SEIKYUKBN       PIC  X(01).
      *
      *�ݸ��ȹ礻�ֹ�
           03  RECE41-HKNCOMBI            PIC  9(04).
      *
      *�����ʬ
           03  RECE41-BYOTOKBN            PIC  X(02).
      *
      *������
           03  RECE41-JNISSU              PIC  9(03).
      *
      *���Ŷ�ʬ����������
           03  RECE41-SRYKBNINF           OCCURS  50.
      *��������
               05  RECE41-KHNTEN          PIC  9(08).
      *���
               05  RECE41-KAISU           PIC  9(05).
      *��������
               05  RECE41-TENSU           PIC S9(08).
      *������������������
               05  RECE41-TENSU-YAK       PIC S9(08).
      **���ٶ�ۡʼ����ա������ͼ��ѡ�
               05  RECE41-KINGAKU-JYU     PIC  9(09).
      *�������
           03  RECE41-TOTALTEN            PIC S9(08).
      *�����������������
           03  RECE41-TOTALTEN-YAK        PIC  9(08).
      **��׶�ۡʼ����ա������ͼ��ѡ�
           03  RECE41-TOTALKIN-JYU        PIC  9(09).
      *
      *��׶��
           03  RECE41-TOTALKIN            PIC  9(09).
      *��׶�ۡ���������
           03  RECE41-TOTALKIN-YAK        PIC  9(09). 
      *
      *���Ŷ�ʬ�̶�۾���
           03  RECE41-SRYKBNINF2          OCCURS  10.
      *���
               05  RECE41-KAISU2          PIC  9(05).
      *��ƿ�ǯ����
               05  RECE41-SYOSAIYMD2      PIC  X(08).
      *���ٶ��
               05  RECE41-KINGAKU2        PIC  9(09).
      *
      *��׶��
           03  RECE41-TOTALKIN2           PIC  9(09).
      *��׶�ۡʤ���¾��
           03  RECE41-TOTALKIN2-ETC       PIC  9(09).
      *
      *���ǽ���
           03  RECE41-SINDAN-TUU          PIC  9(02).
           03  RECE41-SINDAN-KIN          PIC  9(09).
      *���ٽ���
           03  RECE41-MEISAI-TUU          PIC  9(02).
           03  RECE41-MEISAI-KIN          PIC  9(09).
      **����¾���ۡʼ����ա������ͼ��ѡ�
           03  RECE41-SONOTAKIN-JYU       PIC  9(09).
      *
      *�������������
           03  RECE41-SHOKUJIINF.
      *��������
               05  RECE41-SHOKUJINISSU    PIC  9(02).
      *����������
               05  RECE41-SHOKUJIRYOYOHI  PIC  9(09).
      *ɸ����ô��
               05  RECE41-SHOKUJIFTN      PIC  9(09).
      *ɸ����ô��ʬ
           03  RECE41-SHOKUJIFTNKBN       PIC  X(01).
      *
      *��ǻ��ֶ�ʬ
           03  RECE41-SHOSHINTIME         PIC  X(01).
      *���������ʬ
           03  RECE41-SHOSHIN-NYU         PIC  X(01).
      *���ǯ����
           03  RECE41-SHOSHINYMD          PIC  X(08).
      *�ƿǻ��ֶ�ʬ
           03  RECE41-SAISHINTIME         PIC  X(01).
      *�ƿ�Ŧ�װ�����ʬ
           03  RECE41-SAISHIN-PRTKBN      PIC  X(01).
      *�ƿ�Ŧ�װ�����ʬ��
           03  RECE41-SAISHIN-PRTKBN2     PIC  X(01).
      *��������û�Ŧ�װ�����ʬ
           03  RECE41-GAIRAIKSN-PRTKBN    PIC  X(01).
      *��³�����û�������ʬ
           03  RECE41-KEIZOKU-PRTKBN      PIC  X(01).
      *�ߵް��Ŵ����û���ʬ
           03  RECE41-KYUKYU-PRTKBN       PIC  X(01).
      *�ߵް��Ŵ����û�������
           03  RECE41-KYUKYUYMD           PIC  X(08).
      *��������û�������:������
           03  RECE41-GAITOKU-PRTKBN1     PIC  X(01).
           03  RECE41-GAITOKU-TEN1        PIC  9(03).
           03  RECE41-GAITOKU-KAISU1      PIC  9(02).
      *��������û�������:������
           03  RECE41-GAITOKU-PRTKBN2     PIC  X(01).
           03  RECE41-GAITOKU-TEN2        PIC  9(03).
           03  RECE41-GAITOKU-KAISU2      PIC  9(02).
      *������ʬ
           03  RECE41-KENSAKBN            PIC  X(01).
      *���󸡺�ǯ����
           03  RECE41-KENSAYMD-ZEN        PIC  X(08).
      *�̱�������
           03  RECE41-TUUININF            OCCURS  31.
               05  RECE41-TUUIN-DAY       PIC  X(01).
      *�̱�������
           03  RECE41-TUUIN-KEI           PIC  9(02).
      *���̼굻������������������
           03  RECE41-TENSUINF            OCCURS  31.
               05  RECE41-SYUTEN-JI       PIC  9(09).
               05  RECE41-YKZTEN-JI       PIC  9(09).
      *�����ȥ�����ȥե饰
           03  RECE41-COMCNTFLG           PIC  X(01).
      *�¾����Խ��ե饰������������ܻ�Ƴ������
           03  RECE41-BEDSUFLG            PIC  X(01).
      *�����®���θ����û��ե饰
           03  RECE41-KENTAIKASANFLG      PIC  X(01).
      *���Ļ����Ļ��Υ����ȥե饰
           03  RECE41-NYUYOJIFLG          PIC  X(01).
      *����������������󥻥�����ʣ����ܡ˻���ե饰
           03  RECE41-SYOUNIFLG           PIC  X(01).
      *������žɲ�ͽ�ɥ�������Ƴ���ʣ����ܤ���
      *�����ܤޤǡ˻���ե饰
           03  RECE41-NYUSENFLG           PIC  X(01).
      *
      *�������ơ��֥����
           03  RECE41-JNISSUTBLINF        OCCURS 31.
      *�������ơ��֥�
               05  RECE41-JNISSUTBL       PIC  X(01).
      *
      *�������ơ��֥����
           03  RECE41-NYUINBITBLINF       OCCURS 31.
      *�������ơ��֥�
               05  RECE41-NYUINBITBL      PIC  X(01).
      *
      *�����ե饰��Ʊ���ݸ��Ȥ߹�碌��������ͭ�뤫��
           03  RECE41-NYUINFLG            PIC  X(01).
      *
      *�ٶȾ����ե饰
           03  RECE41-KYUGYOFLG           PIC  X(01).
      *�ٶȴ��ֳ��Ϸ���
           03  RECE41-KYUGYO-STR          PIC  X(04).
      *�ٶȴ��ֽ�λ����
           03  RECE41-KYUGYO-END          PIC  X(04).
      *
      *ϫ�ҥ쥻�ץȲ��
           03  RECE41-RECEKAISU           PIC  9(03).
      *
      *��̳�ҳ���ž��ǯ����
           03  RECE41-TENKIYMD            PIC  X(08).
      *
      *ϫ�ҽ������ʬ�ե饰
           03  RECE41-SYOKAIFLG           PIC  X(01).
      *
      *ϫ�Ҵ����Ľ𥳡���
           03  RECE41-SYOCD               PIC  X(05).
      *
      *������������
           03  RECE41-4001-INF.
      *����ñ��
               05  RECE41-TANKA           PIC  9(02)V99.
      *�����ղû�Ψ
               05  RECE41-KSNRATE         PIC  9(03).
      *������ʸ����������
               05  RECE41-TAXKBN          PIC  X(01).
      *��������������������
               05  RECE41-TAXKBN-KZI      PIC  X(01).
      *�����ո����Ӳû����谷
               05  RECE41-KOTEIKBN        PIC  X(01).
      *ϫ�ҥ쥻�������
               05  RECE41-ROUKAISU        PIC  X(01).
      *�����ն�ʬ
               05  RECE41-JIBAIKBN        PIC  X(01).
      *������ñ��
               05  RECE41-SYUTENTANKA     PIC  9(02).
      *��������ñ��
               05  RECE41-YKZTENTANKA     PIC  9(02).
      *��̳�ҳ���ʬ
               05  RECE41-KOMUKBN         PIC  X(01).
      *��̳�ҳ���ñ��
               05  RECE41-KOMUTENTANKA    PIC  9(02).
      *�ٶȾ������ܴ��ֵ���
               05  RECE41-KYUGYO-PRTKBN   PIC  X(01).
      *ϫ�һ�����ŵ��ض�ʬ
               05  RECE41-RSIIRYOKBN      PIC  X(01).
      *
      *�����������ǻϤޤ륳���ɤ�̵ͭ��ʬ
           03  RECE41-ORIGINALCDKBN       PIC  9(01).
      *
      *�軰�Թ԰ٻ��ΰ���ʬ���ݸ�����
           03  RECE41-HKN-X.
      *����ʬ���ݸ��ֹ�
               05  RECE41-HKNNUM-X        PIC  X(03).
      *����ʬ���ݸ��ɣ�
               05  RECE41-HKNID-X         PIC  9(10).
      *����ʬ�ݸ����ֹ�
               05  RECE41-HKNJANUM        PIC  X(08).
      *����ʬ�ݸ���̾
               05  RECE41-HKNJANAME       PIC  X(200).
      *����ʬ����
               05  RECE41-KIGO            PIC  X(80).
      *����ʬ�ֹ�
               05  RECE41-NUM             PIC  X(80).
      *
      *�軰�Թ԰١ʼҲ��ݸ��ؤ�����ۡ�
           03  RECE41-SKYMONEY            PIC  9(09).
      *�軰�Թ԰١ʴ�����ô��
           03  RECE41-FTNMONEY            PIC  9(09).
      *�軰�Թ԰١ʴ��Գ���
           03  RECE41-FTNRATE             PIC  9(03).
      *�軰�Թ԰٥쥻�ץȸ�ʪ���ճ����ե饰
           03  RECE41-DAISAN-GENBUTU-GAITO
                                          PIC  9(01).
      *�軰�Թ԰ٰ�����ô�����ִ��ե饰
           03  RECE41-DAISAN-CHIKAN       PIC  9(01).
      *
      *�����ա���������ü��������ʬ
           03  RECE41-JIBAI-HASUKBN       PIC  X(01).
      *
      *�����աʿ��ͼ��ˡʣϣã��ͼ�����
      *��׳�
           03  RECE41-JIBAI-GOKEI-BE      PIC  9(09).
           03  RECE41-JIBAI-GOKEI-AF      PIC  9(09).
      *�����
           03  RECE41-JIBAI-SKYKINFLG     PIC  9(01).
           03  RECE41-JIBAI-SKYKIN        PIC  9(09).
      *���γۡ�������
           03  RECE41-JIBAI-JYUKINFLG     PIC  9(01).
           03  RECE41-JIBAI-JYUKIN        PIC  9(09).
           03  RECE41-JIBAI-JYUNAMEFLG    PIC  9(01).
           03  RECE41-JIBAI-JYUNAME       PIC  X(80).
      *�����ռ��ζ�����ִ�����ʬ
           03  RECE41-JIBAI-CHIKANKINFLG  PIC  9(01).
           03  RECE41-JIBAI-CHIKANKIN     PIC  9(09).
           03  RECE41-JIBAI-CHIKANNAMEFLG PIC  9(01).
           03  RECE41-JIBAI-CHIKANNAME    PIC  X(80).
      *
      *�����������������ڤӱ���������������Ķ��ͳ�����Ⱦ���
           03  RECE41-SIPPUCHKFLG         PIC  9(01).
           03  RECE41-SIPPUCHKINF.
             04  RECE41-SIPPUCHK-OCC      OCCURS  10.
               05  RECE41-SIPPUCHK-OCC2   OCCURS  31.
                   07  RECE41-SIPPUCHK    PIC  9(03).
      *
      *�쥻�ץȼ����������顼����
           03  RECE41-ERR-AREA.
               05  RECE41-ERR-ERRKBN      PIC  9(02).
               05  RECE41-ERR-DAY         PIC  9(02).
               05  RECE41-ERR-HKNCOMBINUM PIC  9(04).
               05  RECE41-ERR-TEKSTYMD    PIC  9(08).
               05  RECE41-ERR-TEKEDYMD    PIC  9(08).
      *
      *ǥ�إե饰
           03  RECE41-NINPUFLG            PIC  9(01).
      *
      *�����ե饰
           03  RECE41-KENSA-BNPFLG        PIC  9(01).
           03  RECE41-KENSA-NTPROFLG      PIC  9(01).
           03  RECE41-KENSA-ANPFLG        PIC  9(01).
      *
      *��ϥӥ����
           03  RECE41-RIHAINF-AREA.
      *��ϥӥ껻��ơ��֥룲
               05  RECE41-RIHA2-DAY-AREA.
                 06  RECE41-RIHA2-SINDAI-AREA.
                   07  RECE41-RIHA2-SINDAI      PIC 9(01).
                   07  RECE41-RIHA2-SINYMD-OCC1.
                     08  RECE41-RIHA2-SINYMD1   PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-SINYMD-OCC2.
                     08  RECE41-RIHA2-SINYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-KOKYUKI-AREA.
                   07  RECE41-RIHA2-KOKYUKI     PIC 9(01).
                   07  RECE41-RIHA2-KOKYMD-OCC1.
                     08  RECE41-RIHA2-KOKYMD1   PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-KOKYMD-OCC2.
                     08  RECE41-RIHA2-KOKYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-NOUKE-AREA.
                   07  RECE41-RIHA2-NOUKE       PIC 9(01).
                   07  RECE41-RIHA2-NOUYMD-OCC1.
                     08  RECE41-RIHA2-NOUYMD1   PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-NOUYMD-OCC2.
                     08  RECE41-RIHA2-NOUYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-UNDOKI-AREA.
                   07  RECE41-RIHA2-UNDOKI      PIC 9(01).
                   07  RECE41-RIHA2-UNDYMD-OCC1.
                     08  RECE41-RIHA2-UNDYMD1   PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-UNDYMD-OCC2.
                     08  RECE41-RIHA2-UNDYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-NANBYO-AREA.
                   07  RECE41-RIHA2-NANBYO      PIC 9(01).
                   07  RECE41-RIHA2-NANYMD-OCC1.
                     08  RECE41-RIHA2-NANYMD1   PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-NANYMD-OCC2.
                     08  RECE41-RIHA2-NANYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-SYOGAI-AREA.
                   07  RECE41-RIHA2-SYOGAI      PIC 9(01).
                   07  RECE41-RIHA2-SYOYMD-OCC1.
                     08  RECE41-RIHA2-SYOYMD1   PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-SYOYMD-OCC2.
                     08  RECE41-RIHA2-SYOYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-SESSYOKU-AREA.
                   07  RECE41-RIHA2-SESSYOKU    PIC 9(01).
                   07  RECE41-RIHA2-SESYMD-OCC1.
                     08  RECE41-RIHA2-SESYMD1   PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-SESYMD-OCC2.
                     08  RECE41-RIHA2-SESYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-HAIYO-AREA.
                   07  RECE41-RIHA2-HAIYO       PIC 9(01).
                   07  RECE41-RIHA2-HAIYMD-OCC1.
                     08  RECE41-RIHA2-HAIYMD1   PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-HAIYMD-OCC2.
                     08  RECE41-RIHA2-HAIYMD2   PIC 9(01)  OCCURS 31.
      *��ϥӥ�û�����ơ��֥룲
               05  RECE41-RIHA2-DAY9-AREA.
                 06  RECE41-RIHA2-SINDAI9-AREA.
                   07  RECE41-RIHA2-SINDAI9     PIC 9(01).
                   07  RECE41-RIHA2-SIN9YMD-OCC1.
                     08  RECE41-RIHA2-SIN9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-SIN9YMD-OCC2.
                     08  RECE41-RIHA2-SIN9YMD2  PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-KOKYUKI9-AREA.
                   07  RECE41-RIHA2-KOKYUKI9    PIC 9(01).
                   07  RECE41-RIHA2-KOK9YMD-OCC1.
                     08  RECE41-RIHA2-KOK9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-KOK9YMD-OCC2.
                     08  RECE41-RIHA2-KOK9YMD2  PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-HAIYO9-AREA.
                   07  RECE41-RIHA2-HAIYO9      PIC 9(01).
                   07  RECE41-RIHA2-HAI9YMD-OCC1.
                     08  RECE41-RIHA2-HAI9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-HAI9YMD-OCC2.
                     08  RECE41-RIHA2-HAI9YMD2  PIC 9(01)  OCCURS 31.
      *
      *������Ǣ��Ω��Ƴ���γ�����ǽ�λ�����
           03  RECE41-GAIJIRYMD           PIC  X(08).
      *
      *ͽ��
           03  RECE41-YOBIETC-AREA.
               05  RECE41-YOBIETCINF           OCCURS  10.
                   07  RECE41-YOBIETC     PIC  9(09).
      *