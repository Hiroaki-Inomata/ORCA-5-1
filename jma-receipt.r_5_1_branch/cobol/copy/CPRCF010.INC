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
      *    �������           COPY (CPRCF010.INC)                *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     01/08/08  �����ѹ��ܤ��ɲ�
      *                                     �����ѹ��ܤ��ɲ� 
      *  01.00.02    MCC-ƣ��     02/03/15  �����ѹ��ܤ��ɲ�
      *
      *  01.01.01    NACL-ƣ��    02/09/17  ���⸩����ʬ�����ֹ桦
      *                                     ǯ�𡦵��ճ����ɲ�
      *  01.01.02    NACL-ƣ��    02/10/07  �����Ƕ�ʬ���ɲ�
      *  01.01.03    NACL-ƣ��    03/03/06  �����ʬ���ɲ�
      *                                     �����Υ������ѹ�
      *                                     �쥻�ץȵ��ܳ���������ɲ�
      *  01.01.04    NACL-ƣ��    03/04/11  ����ǯ����ѹ��ܤ��ɲ�
      *  01.01.05    NACL-ƣ��    03/05/19  �о��ݸ����ֹ���ɲ�
      *  01.01.06    NACL-����    05/03/11  ��ô��۾���˹����ɲ�
      *                                     ������ô�⡦����������ɸ��
      *                                     ��ô�۶�ʬ�ɲ�
      *                                     ���ܳ��������ô��ۤ˹���
      *                                     �ɲ�
      *
      *  03.04.01    NACL-ƣ��    07/02/27  �ݸ��ɣġ�Ϸ���ݸ��ɣġ�
      *                                     ����ɣĤ��ɲ�
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *  03.05.02    NACL-ƣ��    07/07/18  �õ����ࡢ
      *                                     �����ʬ�ʥ쥻���оݡˤ��ɲ�
      *
      *  04.01.01    NACL-ƣ��    07/11/22  �����쥻�ץ��б�
      *                                     ������ʧ���ظ����ѹ��ܤ��ɲ�
      *
      *  04.02.01    NACL-ƣ��    08/03/12  �ե饰���ǡ������ꥢ�ɲ�
      *
      *  04.04.01    NACL-ƣ��    08/12/17  ����ؼ��ե饰�ɲ�
      *
      *  04.05.01    NACL-ƣ��    09/11/13  �õ�����Ȳ��б�
      *                                     �õ�����ά���ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *                                     Ĵ���Ѥ�crehms,updhms �ɲ�
      *  04.06.02    NACL-ƣ��    10/11/30  ����Ģɼ�Ѥι����ɲ�
      *                                     ����۾���ʰ����񡢿�����
      *                                     �ݸ��ȹ礻����
      *  04.06.03    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *                                     receipt_kbn �ɲ� 
      *
      *  04.07.01    NACL-ƣ��    11/09/02  ϫ�����������б�
      *  04.07.02    NACL-ƣ��    12/02/13  �����ȹ�ؤ�ľ������ȿ���
      *                                     ��ʧ���������б�
      *                                     ľ������ե饰���ɲ�
      *  04.07.03    NACL-ƣ��    13/09/20  �軰�Թ԰��б�
      *
      *  04.08.01    NACL-ƣ��    17/04/10  ����ñ�̤Ǥ�������������
      *                                     ��ϫ�ҽ�����Ϸ��ݽ����б�
      *                                     �ݸ���ʬ�ʼ������ѡˤ��ɲ�
      ************************************************************
       01  RECE10-REC.
      *�ˣţ�
           03  RECE10-KEY.
      *���ŵ��أɣ�
               05  RECE10-HOSPNUM          PIC 9(2).
      *����ǯ��
               05  RECE10-SRYYM            PIC  9(06).
      *������ʬ
               05  RECE10-NYUGAIKBN        PIC  X(01).
      *���ԣɣ�
               05  RECE10-PTID             PIC  9(10).
      *�쥻�ץȲʡʶ���λ������򥻥åȡ�
               05  RECE10-RECEKA           PIC  X(02).
      *������ʧ����
               05  RECE10-TEISYUTUSAKI     PIC  9(01).
      *�쥻�ץȼ���
               05  RECE10-RECESYUBETU      PIC  X(04).
      *�ݸ����ֹ�
               05  RECE10-HKNJANUM         PIC  X(08).
      *�����ʬ�������ݸ��ΤȤ��Τߡ�
               05  RECE10-HOJOKBN-KEY      PIC  X(01).
      *����ɣġʰ��ݡ�����ñ�ȤΤߡ�ϫ�ҡ������ա��ݸ��ɣġ�
               05  RECE10-KOHID-KEY        PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  RECE10-TEKSTYMD         PIC  X(08).
      *ʬ���ʬ
               05  RECE10-RECEIPT-KBN      PIC  X(10).
      *�����ֹ�
           03  RECE10-PTNUM                PIC  X(20).
      *���⡦������ʬ�ʣ������⡡������������
           03  RECE10-PREFKBN              PIC  X(01).
      *���ֹ�
           03  RECE10-PREFNUM              PIC  X(02).
      *ǯ��
           03  RECE10-AGE                  PIC  9(03).
      *���ճ��
           03  RECE10-KYURATE              PIC  9(03).
      *�����Ƕ�ʬ
           03  RECE10-ZAITAKU              PIC  X(01).
      *����ǯ��
           03  RECE10-SKYYM                PIC  X(06).
      *����ǯ��
           03  RECE10-HENREIYM             PIC  X(06).
      *�����ʬ
           03  RECE10-SKYKBN               PIC  X(01).
      *�����ʬ�ʥ쥻���оݡ�
           03  RECE10-SKYKBN-RECEDEN       PIC  X(01).
      *������ʬ
           03  RECE10-TEISEIKBN            PIC  X(01).
      *���ݸ����ݸ��ֹ�
           03  RECE10-HKNNUM               PIC  X(03).
      *���ݸ����ݸ��ɣ�
           03  RECE10-HKNID                PIC  9(10).
      *���ݸ����ܿͲ�²��ʬ
           03  RECE10-HONKZKKBN            PIC  X(01).
      *���ݸ��������ʬ
           03  RECE10-HOJOKBN              PIC  X(01).
      *���ݸ��ݷ�³��ʬ
           03  RECE10-CONTKBN              PIC  X(01).
      *���ݸ��ݵ���
           03  RECE10-KIGO                 PIC  X(80).
      *���ݸ����ֹ�
           03  RECE10-NUM                  PIC  X(80).
      *Ϸ���ݸ����ݸ��ֹ�
           03  RECE10-RJNHKNNUM            PIC  X(03).
      *Ϸ���ݸ����ݸ��ɣ�
           03  RECE10-RJNHKNID             PIC  9(10).
      *�������
           03  RECE10-KOHINF               OCCURS  4.
      *������ݸ��ֹ�
               05  RECE10-KOHNUM           PIC  X(03).
      *�������
           03  RECE10-KOHIDINF             OCCURS  4.
      *����ݸ���ɣ�
               05  RECE10-KOHID            PIC  9(10).
      *����������
           03  RECE10-JNISSUINF            OCCURS  5.
      *������
               05  RECE10-JNISSU           PIC  9(02).
      *���Ŷ�ʬ����������
           03  RECE10-SRYKBNINF           OCCURS  45.
      *��������
               05  RECE10-KHNTEN          PIC  9(08).
      *���
               05  RECE10-KAISU           PIC  9(05).
      *������������
               05  RECE10-TENSUINF        OCCURS  5.
      *��������
                   07  RECE10-TENSU       PIC  S9(08).
      *�����������
           03  RECE10-TOTALTENINF         OCCURS  5.
      *�������
               05  RECE10-TOTALTEN        PIC S9(08).
      *��ô��۾���
           03  RECE10-FTNMONEYINF         OCCURS  5.
      *��ô���
               05  RECE10-FTNMONEY        PIC S9(09).
      *��ô��ۡʰ�����ô�ۡ�
               05  RECE10-FTNMONEY-FTN    PIC S9(09).
      *��ô��ۡʹ����ۡ�
               05  RECE10-FTNMONEY-KOJ    PIC S9(09).
      *
      *�������������
           03  RECE10-SHOKUJIINF          OCCURS  5.
      *��������
               05  RECE10-SHOKUJINISSU    PIC  9(02).
      *����������
               05  RECE10-SHOKUJIRYOYOHI  PIC  9(09).
      *ɸ����ô��
               05  RECE10-SHOKUJIFTN      PIC  9(09).
      *
      *������ô�⡦����������ɸ����ô�۶�ʬ
           03  RECE10-SHOKUJIFTNKBN       PIC  X(01).
      *
      *���ް�����ô�����
           03  RECE10-YKZFTNINF           OCCURS  5.
      *���ް�����ô��
               05  RECE10-YKZFTN          PIC  9(05).
      *�쥻�ץȵ��ܳ��������
           03  RECE10-ETC-KOH-G.
      *���ܳ�������ݸ��ֹ�
               05  RECE10-ETC-KOHINF.
                   07  RECE10-ETC-KOHNUM   PIC  X(03)   OCCURS  4.
      *���ܳ�����ݼ�����
               05  RECE10-ETC-JNISSUINF.
                   07  RECE10-ETC-JNISSU   PIC  9(02)   OCCURS  4.
      *���ܳ�����ݹ������
               05  RECE10-ETC-TOTALTENINF.
                   07  RECE10-ETC-TOTALTEN PIC S9(08)   OCCURS  4.
      *���ܳ��������ô���
               05  RECE10-ETC-FTNMONEYINF               OCCURS  4.
                   07  RECE10-ETC-FTNMONEY     PIC S9(09).
                   07  RECE10-ETC-FTNMONEY-FTN PIC S9(09).
                   07  RECE10-ETC-FTNMONEY-KOJ PIC S9(09).
      *���ܳ���������ް�����ô�����
               05  RECE10-ETC-YKZFTNINF.
                   07  RECE10-ETC-YKZFTN   PIC  9(05)   OCCURS  4.
      *���ܳ�����ݿ������������
               05  RECE10-ETC-SHOKUJIINF       OCCURS  4.
      *��������
                   07  RECE10-ETC-SHOKUJINISSU PIC  9(02).
      *����������
                   07  RECE10-ETC-SHOKUJIRYOYOHI
                                               PIC  9(09).
      *ɸ����ô��
                   07  RECE10-ETC-SHOKUJIFTN   PIC  9(09).
      *
      *����۾���ʰ������
           03  RECE10-SKYIRYO-INF.
      *������
               05  RECE10-SKYIRYO-NISSU    PIC 9(02).
      *������
               05  RECE10-SKYIRYO-TOTALTEN PIC S9(08).
      *������ô
               05  RECE10-SKYIRYO-ICHIBUFTN
                                           PIC S9(09).
      *�ݸ�����ô
               05  RECE10-SKYIRYO-HOKENJA  PIC S9(09).
      *������ô
               05  RECE10-SKYIRYO-KOHFTN   PIC S9(09).
      *
      *����۾���ʿ�����
           03  RECE10-SKYSKJ-INF.
      *���
               05  RECE10-SKYSKJ-KAISU     PIC 9(02).
      *�����
               05  RECE10-SKYSKJ-RYOYOHI   PIC 9(09).
      *������ô
               05  RECE10-SKYSKJ-ICHIBUFTN PIC 9(09).
      *�ݸ�����ô
               05  RECE10-SKYSKJ-HOKENJA   PIC 9(09).
      *������ô
               05  RECE10-SKYSKJ-KOHFTN    PIC 9(09).
      *�ݸ��ȹ礻����
           03  RECE10-HKNCOMBI-INF.
               05  RECE10-HKNCOMBI         PIC 9(04)   OCCURS 10.
      *�õ�����
           03  RECE10-TOKKIINF.
               05  RECE10-TOKKI            PIC X(02)   OCCURS  5.
      *�õ�����ά��
           03  RECE10-TOKKINMINF.
               05  RECE10-TOKKINM          PIC X(12)   OCCURS  5.
      *ϫ�Ҿ���
           03  RECE10-RSIINF.
      *�ݸ���ʬ�ʣ���û�����������¡��������ե���������������̳�ҳ�
      *          ���������ա�ϫ�ҽ��ˡ����������աʷ��ݽ��ˡ�
               05  RECE10-HKNKBN           PIC X(01).
      *�ͼ���ʬ�ʣ������ͼ������������ͼ��������軰�Թ԰١�
               05  RECE10-RECEKBN          PIC X(01).
      *ϫ�ҽ������ʬ�ե饰�ʣ����������ʬ��
               05  RECE10-SYOKAIFLG        PIC X(01).
      *ϫƯ�����Ľ𥳡���
               05  RECE10-SYOCD            PIC X(05).
      *���Ը����ֹ�
               05  RECE10-KOFUNUM          PIC X(14).
      *�ҳ���ʬ�ʣ�����̳�������̶С�
               05  RECE10-SAIGAIKBN        PIC X(01).
      *ϫ�ҥ�����
               05  RECE10-COMMENT          PIC X(200).
      *�ե饰
           03  RECE10-FLGINF.
               05  RECE10-FLG              PIC 9(01)   OCCURS  5.
      *�ǡ������ꥢ
           03  RECE10-RESERVE-AREA         PIC X(50).
      *����ؼ��ե饰
           03  RECE10-HENREI-SIJI-FLG      PIC 9(01).
      *ľ������ե饰
           03  RECE10-DIRECT-FLG           PIC X(01).
      *�ݸ���ʬ�ʣ��������ա�ϫ�ҽ��ˡ����������աʷ��ݽ��ˡ�
           03  RECE10-HKNKBN-RSI           PIC X(01).
      *(����ɽ��)
      *���ݸ����ݸ��ֹ�
           03  RECE10-HKNNUM-S             PIC X(03).
      *�ݸ����ֹ� 
           03  RECE10-HKNJANUM-S           PIC X(08).
      *��Ͽǯ����
           03  RECE10-CREYMD               PIC X(08).
      *��Ͽ����
           03  RECE10-CREHMS               PIC X(06).
      *����ǯ����
           03  RECE10-UPDYMD               PIC X(08).
      *��������
           03  RECE10-UPDHMS               PIC X(06).
      *
      *�祭�������ѹ���
           03  RECE10-UPHKNJANUM           PIC X(10).
      *�ݸ����ֹ渡���ѹ���
           03  RECE10-XXPREFNUM            PIC X(02).
      *����ǯ����ѹ���
           03  RECE10-XXSRYYM              PIC 9(06).
      *�о��ݸ����ֹ渡���ѹ��� 
           03  RECE10-XXSTHKNJANUM         PIC X(08).
           03  RECE10-XXEDHKNJANUM         PIC X(08).
      *������ʧ���ظ����ѹ���
           03  RECE10-XXTEISYUTUSAKI       PIC 9(01).