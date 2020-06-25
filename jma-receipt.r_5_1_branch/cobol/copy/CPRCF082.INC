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
      *    ϫ�ҥ쥻�ץ����ٽ��������     COPY (CPRCF082.INC)    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.01.01   NACL-����     05.01.07  ������ô���女���ɡ���������
      *                                     �����̾���ɲ�
      *  01.01.02   NACL-����     05.01.26  ��������������ɲ�
      *  01.01.03   NACL-����     05.08.02  �����ս����ͼ��ѹ����ɲ�
      *  01.01.04   NACL-����     05.12.01  ϫ�ҥ쥻������ܡ����ǯ��
      *                                     ϫ�ҥ쥻������ܡ����
      *                                     ϫ�ҥ쥻�ץȲ��
      *                                     ϫ�ҥ쥻������ܤ��ɲ�
      *  01.01.05   NACL-����     06.02.07  ��������������ʤ���¾���ɲ�
      *
      *  02.09.01   NACL-ƣ��     06.04.14  ��������ǯ��������б�
      *                                     �����®���θ����û��ե饰�ɲ�
      *  03.03.01   NACL-����     06.10.10  ���������������񿩻��б�
      *  03.04.01   NACL-����     06.12.01  �����շ��ݽ���б�
      *  03.05.01   NACL-����     07.06.01  ���롼�׿����б�
      *  03.05.02   NACL-����     07.06.26  ������ʿ������ǯ��������ͼ��б�
      *
      *  04.01.01   NACL-����     07/10/25  ��̳�ҳ��б�
      *  04.01.02   NACL-����     07/12/19  ������̤�������б�
      *  04.02.01   NACL-����     08/02/26  �����ʬ�ɲ�
      *
      *  04.04.01   NACL-ƣ��     09/03/10  ���̤ʴط��ˤ���¾�������
      *                                     �������Ȥ��Ƶ������쥻�ץȵ�
      *                                     �ܤ�Ԥʤ��б�
      *  04.05.01   NACL-����     10/01/25  ���̼굻���˥������ɲ�
      *  04.05.02   NACL-����     10/02/16  ͽ�������ɲ�
      *  04.05.03   NACL-ƣ��     11/11/22  Ʊ���������б�
      *  04.05.04   NACL-����     12/04/09  ����ά�Τ�ʸ������ĥ
      *
      *  04.07.01   NACL-����     11/09/01  ϫ�ҽ������ʬ�ե饰�ɲ�
      *                                     ϫ�Ҵ����Ľ𥳡����ɲ�
      *                                     �ƥ��ȴ����ֹ��ʬ�ɲ�
      *  04.07.02    NACL-����    13/01/21  ϫ�ҥ쥻���б�
      *  04.07.03    NACL-����    13/04/16  ϫ�ҥ쥻���б�
      *                                     ϫ�һ�����ŵ��ض�ʬ�ɲ�
      *  04.07.04    NACL-����    13/08/20  �����������ǻϤޤ륳���ɤ�
      *                                     ̵ͭ��ʬ�ɲ�
      *  04.07.05    NACL-ƣ��    13/09/20  �軰�Թ԰��б�
      *  04.07.06    NACL-����    14/05/13  Ķ�žɻ��ʼԡ��������Ųû�����
      *                                     ����ե饰�ɲ�
      *  04.07.07    NACL-����    14/07/31  �軰�Թ԰٤˴ؤ���ե饰�ɲ�
      *  04.07.08    NACL-����    15/09/08  �����աʿ��ͼ��ˡʣϣã��ͼ���
      *                                     �Ѥι����ɲ�
      *
      *  04.08.01    NACL-����    15/08/27  �����ա���������ü������
      *                                     ��ʬ�ɲ�
      *  04.08.02    NACL-����    16/04/15  ʿ������ǯ��������б�
      *  04.08.03    NACL-����    17/02/24  �쥻�ץȼ����������顼������
      *  04.08.04    NACL-����    18/04/06  ʿ������ǯ��������б�
      *
      *  05.00.01    NACL-����    19/07/01  ��ϥӥ�ȯ�����쥻�ץȵ����б�
      *                                     �ʵ�ǽ�ɲá�
      ************************************************************
      *01  RECE82-REC.
           03  RECE82-KEY.
      *���ŵ��أɣ�
               05  RECE82-HOSPNUM         PIC  9(02).
      *����ǯ��
               05  RECE82-SRYYM           PIC  9(06).
      *������ʬ
               05  RECE82-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  RECE82-PTID            PIC  9(10).
      *�쥻�ץȲ�
               05  RECE82-RECEKA          PIC  X(02).
      *�ݸ��ɣ�        
               05  RECE82-HKNID           PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  RECE82-TEKSTYMD        PIC  X(08).
      *�쥻�ץȼ���
           03  RECE82-RECESYUBETU         PIC  9(04).
      *�����ֹ�
           03  RECE82-PTNUM               PIC  X(20).
      *���Ųʾ���
           03  RECE82-SRYKAINF            OCCURS  20.
      *���Ų�
               05  RECE82-SRYKA           PIC  X(02).
      *���ʻ�̾
           03  RECE82-KANANAME            PIC  X(50).
      *������̾
           03  RECE82-NAME                PIC  X(100).
      *����
           03  RECE82-SEX                 PIC  X(01).
      *��ǯ����
           03  RECE82-BIRTHDAY            PIC  9(08).
      *��˴��ʬ
           03  RECE82-DEATHKBN            PIC  X(01).
      *ǯ��
           03  RECE82-AGE                 PIC  9(03).
      *�ƥ��ȴ����ֹ��ʬ
           03  RECE82-TSTPTNUMKBN         PIC  X(01).
      *
      *���ݸ�����
           03  RECE82-HKN.
      *���ݸ���ϫ���ݸ���ʬ
               05  RECE82-HKNKBN          PIC  X(01).
      *���ݸ����ݸ��ֹ�
               05  RECE82-HKNNUM          PIC  X(03).
      *���ݸ��ݴ��Ը����ֹ�
               05  RECE82-KOFUNUM         PIC  X(14).
      *���ݸ��ݻ��Ƚ�̾��
               05  RECE82-JIGYOUNAME      PIC  X(80).
      *���ݸ��ݽ������ƻ�ܸ�̾
               05  RECE82-PREFNAME        PIC  X(20).
      *���ݸ��ݽ������ƻ�ܸ���ʬ
               05  RECE82-PREFKBN         PIC  X(01).
      *���ݸ��ݽ���Ϸ��Զ�̾
               05  RECE82-CITYNAME        PIC  X(80).
      *���ݸ��ݽ���Ϸ��Զ��ʬ
               05  RECE82-CITYKBN         PIC  X(01).
      *���ݸ��ݿ��Ѻ���
               05  RECE82-SINKEI          PIC  X(01).
      *���ݸ���ž����ͳ
               05  RECE82-TENKI           PIC  X(01).
      *���ݸ��ݽ���ǯ����
               05  RECE82-SHOBYOYMD       PIC  X(08).
      *���ݸ��ݻͻ������ʬ
               05  RECE82-SISIKBN         PIC  X(01).
      *���ݸ������ܳ���ǯ����
               05  RECE82-RYOSTYMD        PIC  X(08).
      *���ݸ������ܽ�λǯ����
               05  RECE82-RYOEDYMD        PIC  X(08).
      *���ݸ���ϫ�ҥ�����
               05  RECE82-COMMENT         PIC  X(200).
      *���ݸ��ݺҳ���ʬ
               05  RECE82-SAIGAIKBN       PIC  X(01).
      *���ݸ��ݼ�����ô���女����
               05  RECE82-DRCD            PIC  X(05).
      *���ݸ��ݼ������ݸ����̾
               05  RECE82-HOKENNAME       PIC  X(80).
      *���ݸ���ϫ�ҥ쥻������ܡ����ǯ��
               05  RECE82-KIJYUNYM-KSI    PIC  X(06).
      *���ݸ���ϫ�ҥ쥻������ܡ����
               05  RECE82-KAISU-KSI       PIC  9(03).
      *���ݸ��������ʬ
               05  RECE82-SEIKYUKBN       PIC  X(01).
      *
      *�ݸ��ȹ礻�ֹ�
           03  RECE82-HKNCOMBI            PIC  9(04).
      *
      *������
           03  RECE82-JNISSU              PIC  9(03).
      *
      *���Ŷ�ʬ����������
           03  RECE82-SRYKBNINF           OCCURS  50.
      *��������
               05  RECE82-KHNTEN          PIC  9(08).
      *���
               05  RECE82-KAISU           PIC  9(05).
      *��������
               05  RECE82-TENSU           PIC  9(08).
      *������������������
               05  RECE82-TENSU-YAK       PIC  9(08).
      **���ٶ�ۡʼ����ա������ͼ��ѡ�
               05  RECE82-KINGAKU-JYU     PIC  9(09).
      *
      *����ǯ����
           03  RECE82-NYUINYMD            PIC  X(08).
      *
      *���ŵ��ؼ���
           03  RECE82-HOSPSBT             PIC  X(01).
      *
      *�����ʬ����
           03  RECE82-BYOTOKBNINF         OCCURS  4.
      *�����ʬ
               05  RECE82-BYOTOKBN        PIC  X(02).
      *
      *������̾���
           03  RECE82-BYOTOSYUBETUINF     OCCURS 31.
      *�����ֹ�
               05  RECE82-BYOTONUM        PIC  X(02).
      *�������
               05  RECE82-BYOTOSYUBETU    PIC  X(02).
      *
      *�������������̾���
           03  RECE82-NYUSYUINF           OCCURS  20.
      *��������������
               05  RECE82-NYUSYUBETU      PIC  X(20).
      *
      *��������������
           03  RECE82-NYUKHNINF           OCCURS  20.
      *�������������û�
               05  RECE82-NYUKHNRYO       PIC  9(08).
      *��������
               05  RECE82-NYUKHNNISSU     PIC  9(02).
      *�������
               05  RECE82-NYUKHNKEI       PIC  9(08).
      **��׶�ۡʼ����ա������ͼ��ѡ�
               05  RECE82-NYUKHNKIN-JYU   PIC  9(09).
      *��������������
           03  RECE82-NYUKHNINF2.
      *���������
               05  RECE82-NYUKHNKEI2      PIC S9(08).
      **��׶�ۣ��ʼ����ա������ͼ��ѡ�
               05  RECE82-NYUKHNKIN2-JYU  PIC S9(09).
      *
      *������������������
           03  RECE82-NYUTOKINF           OCCURS  20.
      *����������������¾������
               05  RECE82-NYUTOKRYO       PIC  9(08).
      *����������������¾��ά�Ρ�
               05  RECE82-NYUTOKRYO-RYAKU PIC  X(20).
      *��������
               05  RECE82-NYUTOKNISSU     PIC  9(02).
      *�������
               05  RECE82-NYUTOKKEI       PIC  9(08).
      **��׶�ۡʼ����ա������ͼ��ѡ�
               05  RECE82-NYUTOKKIN-JYU   PIC  9(09).
      *������������������
           03  RECE82-NYUTOKINF2.
      *���������
               05  RECE82-NYUTOKKEI2      PIC S9(08).
      **��׶�ۣ��ʼ����ա������ͼ��ѡ�
               05  RECE82-NYUTOKKIN2-JYU  PIC S9(09).
      *
      *����������������
           03  RECE82-NYUTOTALTEN         PIC  9(08).
      **������������۷סʼ����ա������ͼ��ѡ�
           03  RECE82-NYUTOTALKIN-JYU     PIC  9(09).
      *
      *�������
           03  RECE82-TOTALTEN            PIC  9(08).
      *�����������������
           03  RECE82-TOTALTEN-YAK        PIC  9(08).
      **��׶�ۡʼ����ա������ͼ��ѡ�
           03  RECE82-TOTALKIN-JYU        PIC  9(09).
      *
      *���Ŷ�ʬ�̶�۾���
           03  RECE82-SRYKBNINF2          OCCURS  5.
      *���ٶ��
               05  RECE82-KINGAKU2        PIC  9(09).
      *����
           03  RECE82-KINGAKU2-SYO        PIC  9(09).
      *
      *���Ŷ�ʬ�̶�۾���ʤ���¾�ˡʼ����դΤߡ�
           03  RECE82-SRYKBNINF3          OCCURS  5.
      *���ٶ�ۡʤ���¾��
               05  RECE82-KINGAKU3        PIC  9(09).
      *���סʤ���¾��
           03  RECE82-KINGAKU3-SYO        PIC  9(09).
      *���������û��ʤ���¾��
           03  RECE82-HEYA-NINZU          PIC  9(02).
           03  RECE82-HEYA-NISSU          PIC  9(02).
      *
      *�������̾���
           03  RECE82-SHOKUJISYUINF       OCCURS   4.
      *��������
               05  RECE82-SHOKUJISYUBETU  PIC  X(04).
      *
      *��������������
           03  RECE82-SHOKUJIKHNINF       OCCURS   4.
      *����������
               05  RECE82-SHOKUJIKHNRYO   PIC  9(09).
      *��������
               05  RECE82-SHOKUJIKHNNISSU PIC  9(02).
      *
      *�������̾��󡾴��ܡ������
           03  RECE82-SHOKUJISYUBETU-SEI  PIC  X(02).
      *
      *��������������������
           03  RECE82-SHOKUJIKHNINF-SEI   OCCURS   2.
      *����������
               05  RECE82-SHOKUJIKHNRYO-SEI   PIC  9(09).
      *��������
               05  RECE82-SHOKUJIKHNNISSU-SEI PIC  9(02).
      *
      *��������������ʤ���¾��
           03  RECE82-SHOKUJIKHNINF-ETC       OCCURS   3.
      *����������
               05  RECE82-SHOKUJIKHNRYO-ETC   PIC  9(09).
      *��������
               05  RECE82-SHOKUJIKHNNISSU-ETC PIC  9(02).
      *��������ʿ�����
           03  RECE82-SHOKUJITOTALDAY     PIC  9(02).
      *��׶�ۡʿ����ˡʥ����ϥۡ�
           03  RECE82-SHOKUJITOTALKIN     PIC  9(09).
      *
      *���ǽ���
           03  RECE82-SINDAN-TUU          PIC  9(02).
           03  RECE82-SINDAN-KIN          PIC  9(09).
      *���ٽ���
           03  RECE82-MEISAI-TUU          PIC  9(02).
           03  RECE82-MEISAI-KIN          PIC  9(09).
      **���������û���ۡʼ����ա������ͼ��ѡ�
           03  RECE82-SITURYO-JYU         PIC  9(09).
      **����¾���ۡʼ����ա������ͼ��ѡ�
           03  RECE82-SONOTAKIN-JYU       PIC  9(09).
      *
      *��׶�ۡʥ����ϣ���
           03  RECE82-TOTALKIN            PIC  9(09).
      *��׶�ۡ��������ˡʣ¡�
           03  RECE82-TOTALKIN-YAK        PIC  9(09).
      *��׶�ۡʣá�
           03  RECE82-TOTALKIN2           PIC  9(09).
      *��׶�ۡ�¾�ˡʣġ�
           03  RECE82-TOTALKIN-ETC        PIC  9(09).
      *
      *��ǻ��ֶ�ʬ
           03  RECE82-SHOSHINTIME         PIC  X(01).
      *���������ʬ
           03  RECE82-SHOSHIN-NYU         PIC  X(01).
      *���ǯ����
           03  RECE82-SHOSHINYMD          PIC  X(08).
      *�ߵް��Ŵ����û���ʬ
           03  RECE82-KYUKYU-PRTKBN       PIC  X(01).
      *�ߵް��Ŵ����û�������
           03  RECE82-KYUKYUYMD           PIC  X(08).
      *���̼굻������������������
           03  RECE82-TENSUINF            OCCURS  31.
               05  RECE82-SYUTEN-JI       PIC S9(08).
               05  RECE82-YKZTEN-JI       PIC  9(08).
      *�����ȥ�����ȥե饰
           03  RECE82-COMCNTFLG           PIC  X(01).
      *�¾����Խ��ե饰������������ܻ�Ƴ������
           03  RECE82-BEDSUFLG            PIC  X(01).
      *�����®���θ����û��ե饰
           03  RECE82-KENTAIKASANFLG      PIC  X(01).
      *���̤ʴط��ˤ���¾���򤢤�ե饰
           03  RECE82-TAINFLG             PIC  X(01).
      *���Ų���ơ��֥�
           03  RECE82-DAY-AREA.
               05  RECE82-DAY             PIC  9(03)
                                               OCCURS   31.
      *�쥻�ץȼ����������顼����
           03  RECE82-ERR-AREA.
               05  RECE82-ERR-ERRKBN      PIC 9(02).
               05  RECE82-ERR-DAY         PIC 9(02).
               05  RECE82-ERR-HKNCOMBINUM PIC 9(04).
               05  RECE82-ERR-TEKSTYMD    PIC 9(08).
               05  RECE82-ERR-TEKEDYMD    PIC 9(08).
      *¾�ݸ�����������Ѥߥե饰
           03  RECE82-NYUINRYOFLG         PIC  X(01).
      *
      *ϫ�ҥ쥻�ץȲ��
           03  RECE82-RECEKAISU           PIC  9(03).
      *
      *��̳�ҳ���ž��ǯ����
           03  RECE82-TENKIYMD            PIC  X(08).
      *
      *ϫ�ҽ������ʬ�ե饰
           03  RECE82-SYOKAIFLG           PIC  X(01).
      *
      *ϫ�Ҵ����Ľ𥳡���
           03  RECE82-SYOCD               PIC  X(05).
      *
      *������������
           03  RECE82-4001-INF.
      *����ñ��
               05  RECE82-TANKA           PIC  9(02)V99.
      *�����ղû�Ψ
               05  RECE82-KSNRATE         PIC  9(03).
      *������ʸ����������
               05  RECE82-TAXKBN          PIC  X(01).
      *�����տ���������
               05  RECE82-SKJKBN          PIC  X(01).
      *��������������������
               05  RECE82-TAXKBN-KZI      PIC  X(01).
      *�����ո����Ӳû����谷
               05  RECE82-KOTEIKBN        PIC  X(01).
      *ϫ�ҥ쥻�������
               05  RECE82-ROUKAISU        PIC  X(01).
      *�����ն�ʬ
               05  RECE82-JIBAIKBN        PIC  X(01).
      *������ñ��
               05  RECE82-SYUTENTANKA     PIC  9(02).
      *��������ñ��
               05  RECE82-YKZTENTANKA     PIC  9(02).
      *��̳�ҳ���ʬ
               05  RECE82-KOMUKBN         PIC  X(01).
      *��̳�ҳ���ñ��
               05  RECE82-KOMUTENTANKA    PIC  9(02).
      *ϫ�һ�����ŵ��ض�ʬ
               05  RECE82-RSIIRYOKBN      PIC  X(01).
      *
      *�����������ǻϤޤ륳���ɤ�̵ͭ��ʬ
           03  RECE82-ORIGINALCDKBN       PIC 9(01).
      *
      *��������������ե饰
           03  RECE82-DOUJITSU-FLG        PIC 9(01).
      *����������
           03  RECE82-DOUJITSU-AREA.
      *Ʊ��������������ʬ�ˤ��ݸ��ȹ礻
               05  RECE82-DOUJITSU-HKNCOMBI
                                          PIC 9(04)   OCCURS  31.
      *Ʊ��������������ʬ�ˤο������ϻ��ֶ�ʬ
               05  RECE82-DOUJITSU-SKJKBN PIC X(01)   OCCURS  31.
      *������ʬ
               05  RECE82-NYUINDAY-TBL.
                   07  RECE82-NYUINDAY-KBN
                                          PIC  X(01)  OCCURS  31.
      *
      *�軰�Թ԰ٻ��ΰ���ʬ���ݸ�����
           03  RECE82-HKN-X.
      *����ʬ���ݸ��ֹ�
               05  RECE82-HKNNUM-X        PIC X(03).
      *����ʬ���ݸ��ɣ�
               05  RECE82-HKNID-X         PIC 9(10).
      *����ʬ�ݸ����ֹ�
               05  RECE82-HKNJANUM        PIC X(08).
      *����ʬ�ݸ���̾
               05  RECE82-HKNJANAME       PIC X(200).
      *����ʬ����
               05  RECE82-KIGO            PIC X(80).
      *����ʬ�ֹ�
               05  RECE82-NUM             PIC X(80).
      *
      *�軰�Թ԰١ʼҲ��ݸ��ؤ�����ۡ�
           03  RECE82-SKYMONEY            PIC  9(09).
      *�軰�Թ԰١ʴ�����ô��
           03  RECE82-FTNMONEY            PIC  9(09).
      *�軰�Թ԰١ʴ��Գ���
           03  RECE82-FTNRATE             PIC  9(03).
      *�軰�Թ԰١ʿ���ɸ����ô��
           03  RECE82-SKJFTNMONEY         PIC  9(09).
      *�軰�Թ԰٥쥻�ץȸ�ʪ���ճ����ե饰
           03  RECE82-DAISAN-GENBUTU-GAITO
                                          PIC  9(01).
      *�軰�Թ԰ٰ�����ô�����ִ��ե饰
           03  RECE82-DAISAN-CHIKAN       PIC  9(01).
      *
      *Ķ�žɻ��ʼԡ��������Ųû����λ���ե饰
           03  RECE82-CYOJKSNFLG          PIC  X(01).
      *
      *�����ա���������ü��������ʬ
           03  RECE82-JIBAI-HASUKBN       PIC  X(01).
      *
      *�����աʿ��ͼ��ˡʣϣã��ͼ�����
      *��׳�
           03  RECE82-JIBAI-GOKEI-BE      PIC  9(09).
           03  RECE82-JIBAI-GOKEI-AF      PIC  9(09).
      *�����
           03  RECE82-JIBAI-SKYKINFLG     PIC  9(01).
           03  RECE82-JIBAI-SKYKIN        PIC  9(09).
      *���γۡ�������
           03  RECE82-JIBAI-JYUKINFLG     PIC  9(01).
           03  RECE82-JIBAI-JYUKIN        PIC  9(09).
           03  RECE82-JIBAI-JYUNAMEFLG    PIC  9(01).
           03  RECE82-JIBAI-JYUNAME       PIC  X(80).
      *�����ռ��ζ�����ִ�����ʬ
           03  RECE82-JIBAI-CHIKANKINFLG  PIC  9(01).
           03  RECE82-JIBAI-CHIKANKIN     PIC  9(09).
           03  RECE82-JIBAI-CHIKANNAMEFLG PIC  9(01).
           03  RECE82-JIBAI-CHIKANNAME    PIC  X(80).
      *
      *ɸ����ô�۷в����֡�������
           03  RECE82-SEISIN-SKJFLG       PIC  9(01).
           03  RECE82-SEISIN-NYUINYMD     PIC  X(08).
      *
      *�ƿǲû��������˾���
           03  RECE82-NYUSAIINF.
               05  RECE82-NYUSAI-TENSU    PIC  9(08).
      *���������̿��Ŷ�ʬ����
           03  RECE82-NYUDAYINF.
               05  RECE82-NYUDAY-SRYKBN   PIC  X(02)  OCCURS 31.
      *
      *ǥ�إե饰
           03  RECE82-NINPUFLG            PIC  9(01).
      *
      *�����ե饰
           03  RECE82-KENSA-BNPFLG        PIC  9(01).
           03  RECE82-KENSA-NTPROFLG      PIC  9(01).
           03  RECE82-KENSA-ANPFLG        PIC  9(01).
      *
      *��ϥӥ����
           03  RECE82-RIHAINF-AREA.
      *��ϥӥ껻��ơ��֥룲
               05  RECE82-RIHA2-DAY-AREA.
                 06  RECE82-RIHA2-SINDAI-AREA.
                   07  RECE82-RIHA2-SINDAI      PIC 9(01).
                   07  RECE82-RIHA2-SINYMD-OCC1.
                     08  RECE82-RIHA2-SINYMD1   PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-SINYMD-OCC2.
                     08  RECE82-RIHA2-SINYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-KOKYUKI-AREA.
                   07  RECE82-RIHA2-KOKYUKI     PIC 9(01).
                   07  RECE82-RIHA2-KOKYMD-OCC1.
                     08  RECE82-RIHA2-KOKYMD1   PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-KOKYMD-OCC2.
                     08  RECE82-RIHA2-KOKYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-NOUKE-AREA.
                   07  RECE82-RIHA2-NOUKE       PIC 9(01).
                   07  RECE82-RIHA2-NOUYMD-OCC1.
                     08  RECE82-RIHA2-NOUYMD1   PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-NOUYMD-OCC2.
                     08  RECE82-RIHA2-NOUYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-UNDOKI-AREA.
                   07  RECE82-RIHA2-UNDOKI      PIC 9(01).
                   07  RECE82-RIHA2-UNDYMD-OCC1.
                     08  RECE82-RIHA2-UNDYMD1   PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-UNDYMD-OCC2.
                     08  RECE82-RIHA2-UNDYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-NANBYO-AREA.
                   07  RECE82-RIHA2-NANBYO      PIC 9(01).
                   07  RECE82-RIHA2-NANYMD-OCC1.
                     08  RECE82-RIHA2-NANYMD1   PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-NANYMD-OCC2.
                     08  RECE82-RIHA2-NANYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-SYOGAI-AREA.
                   07  RECE82-RIHA2-SYOGAI      PIC 9(01).
                   07  RECE82-RIHA2-SYOYMD-OCC1.
                     08  RECE82-RIHA2-SYOYMD1   PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-SYOYMD-OCC2.
                     08  RECE82-RIHA2-SYOYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-SESSYOKU-AREA.
                   07  RECE82-RIHA2-SESSYOKU    PIC 9(01).
                   07  RECE82-RIHA2-SESYMD-OCC1.
                     08  RECE82-RIHA2-SESYMD1   PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-SESYMD-OCC2.
                     08  RECE82-RIHA2-SESYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-HAIYO-AREA.
                   07  RECE82-RIHA2-HAIYO       PIC 9(01).
                   07  RECE82-RIHA2-HAIYMD-OCC1.
                     08  RECE82-RIHA2-HAIYMD1   PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-HAIYMD-OCC2.
                     08  RECE82-RIHA2-HAIYMD2   PIC 9(01)  OCCURS 31.
      *��ϥӥ�û�����ơ��֥룲
               05  RECE82-RIHA2-DAY9-AREA.
                 06  RECE82-RIHA2-SINDAI9-AREA.
                   07  RECE82-RIHA2-SINDAI9     PIC 9(01).
                   07  RECE82-RIHA2-SIN9YMD-OCC1.
                     08  RECE82-RIHA2-SIN9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-SIN9YMD-OCC2.
                     08  RECE82-RIHA2-SIN9YMD2  PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-KOKYUKI9-AREA.
                   07  RECE82-RIHA2-KOKYUKI9    PIC 9(01).
                   07  RECE82-RIHA2-KOK9YMD-OCC1.
                     08  RECE82-RIHA2-KOK9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-KOK9YMD-OCC2.
                     08  RECE82-RIHA2-KOK9YMD2  PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-HAIYO9-AREA.
                   07  RECE82-RIHA2-HAIYO9      PIC 9(01).
                   07  RECE82-RIHA2-HAI9YMD-OCC1.
                     08  RECE82-RIHA2-HAI9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-HAI9YMD-OCC2.
                     08  RECE82-RIHA2-HAI9YMD2  PIC 9(01)  OCCURS 31.
      *
      *��Ǣ��Ω�ٱ�û��γ�����ǽ�λ�����
           03  RECE82-HAIJSKYMD                 PIC X(08).
      *
      *ͽ��
           03  RECE82-YOBIETC-AREA.
               05  RECE82-YOBIETCINF           OCCURS  10.
                   07  RECE82-YOBIETC     PIC  9(09).
      *