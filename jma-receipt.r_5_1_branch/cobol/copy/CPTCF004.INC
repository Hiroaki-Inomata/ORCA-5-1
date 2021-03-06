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
      *    �������ץǡ���          COPY (CPTCF004.INC)    *
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.05.01    NACL-ƣ��    12/07/02  �����������������ɲ�
      *                                     X(400) ���ѹ�
      ************************************************************
      *01  TOUKEI04-REC.
      *    �쥳���ɴ��ܾ���
           03  TOUKEI04-KIHON-AREA.
      *        ���ŵ��أɣ�
               05  TOUKEI04-HOSPID         PIC  X(24).
      *        ���ŵ��إ�����
               05  TOUKEI04-HOSPCD         PIC  X(07).
      *        ����ǯ����
               05  TOUKEI04-SRYYMD         PIC  9(08).
      *        �쥳���ɼ���
               05  TOUKEI04-RECSKB         PIC  9(01).
      *        �쥻�ץȼ��̥�����
               05  TOUKEI04-RECESYUBETU    PIC  9(04).
      *        ������ʬ
               05  TOUKEI04-NYUGAIKBN      PIC  9(01).
      *        ���Ų�
               05  TOUKEI04-SRYKA          PIC  X(02).
      *        �쥻�ſ��Ų�
               05  TOUKEI04-RECEKA         PIC  X(02).
      *        �ݸ��ȹ礻�ֹ�
               05  TOUKEI04-HKNCOMBINUM    PIC  9(04).
      *    ���Դ��ܾ���
           03  TOUKEI04-PTKHN-AREA.
      *        �����ֹ�
               05  TOUKEI04-PTNUM          PIC  X(20).
      *        ���ԣɣ�
               05  TOUKEI04-PTID           PIC  X(10).
      *        ���ʻ�̾
               05  TOUKEI04-KANANAME       PIC  X(50).
      *        ������̾
               05  TOUKEI04-NAME           PIC  X(100).
      *        ����
               05  TOUKEI04-SEX            PIC  X(01).
      *        ��ǯ����
               05  TOUKEI04-BIRTHDAY       PIC  9(08).
      *    �������
           03  TOUKEI04-GAIRAIINF-AREA.
      *        ��������ʬ
               05  TOUKEI04-KIHONKBN       PIC  9(01).
      *        ���ֳ���ʬ
               05  TOUKEI04-TIMEKBN        PIC  9(01).
      *        ���Ƕ�ʬ
               05  TOUKEI04-OSINKBN        PIC  9(01).
      *        �Ҳ��ʬ
               05  TOUKEI04-SHOUKAIKBN     PIC  9(01).
      *        �����泰�輱��
               05  TOUKEI04-NYUHAISKB      PIC  X(01).
      *        ��ե�����
               05  TOUKEI04-DRCD           PIC  X(05).
      *    ��������
           03  TOUKEI04-NYUININF-AREA.
      *        ���ౡ��ʬ�ʣ����������������������ౡ�������������
               05  TOUKEI04-NYUIN-KBN      PIC  9(01).
      *        �����ֹ�
               05  TOUKEI04-BTUNUM         PIC  X(02).
      *        �¼��ֹ�
               05  TOUKEI04-BRMNUM         PIC  X(06).
      *        �����ʬ
               05  TOUKEI04-GAIHAKU-KBN    PIC  9(01).
      *        ������ʬ
               05  TOUKEI04-SKJ-KBN        PIC  9(01).
      *        �ౡ��ͳ
               05  TOUKEI04-TAIINCD        PIC  X(02).
      *        �������Ų�
               05  TOUKEI04-MAE-SRYKA      PIC  X(02).
      *        �����쥻�ſ��Ų�
               05  TOUKEI04-MAE-RECEKA     PIC  X(02).
      *        ���������ֹ�
               05  TOUKEI04-MAE-BTUNUM     PIC  X(02).
      *        �����¼��ֹ�
               05  TOUKEI04-MAE-BRMNUM     PIC  X(06).
      *    ��Ǽ�������
           03  TOUKEI04-SYUNOU-AREA.
      *        ��ɼ�ֹ�
               05  TOUKEI04-DENPNUM        PIC  9(07).
      *        ��ɼ���ֶ�ʬ
               05  TOUKEI04-DENPJTIKBN     PIC  X(01).
      *        ��ɼ������ʬ
               05  TOUKEI04-CREATEKBN      PIC  X(01).
      *        �ݸ�����
               05  TOUKEI04-HKNTEN         PIC  9(07).
      *        �ݸ���ô���
               05  TOUKEI04-HKNMONEY       PIC  9(07).
      *        �ݸ�Ŭ�ѳ����
               05  TOUKEI04-TGMONEY        PIC  9(07).
      *        �ݸ�Ŭ�ѳ����ǳ�
               05  TOUKEI04-TGMONEY-TAX    PIC  9(07).
      *        ������
               05  TOUKEI04-JIHI           PIC  9(07).
      *        �����ǳ�
               05  TOUKEI04-JIHI-TAX       PIC  9(07).
      *        ������ô���
               05  TOUKEI04-SKJ            PIC  9(07).
      *        �����ǳ�
               05  TOUKEI04-SKJ-TAX        PIC  9(07).
      *        ��������
               05  TOUKEI04-RMSAGAKU       PIC  9(07).
      *        ���������ǳ�
               05  TOUKEI04-RMSAGAKU-TAX   PIC  9(07).
      *        ������
               05  TOUKEI04-SKYMONEY       PIC  9(07).
      *    �ݸ�����
           03  TOUKEI04-HKN-AREA.
      *        �ݸ��ֹ�
               05  TOUKEI04-HKNNUM         PIC  X(03).
      *        �ݸ��ɣ�
               05  TOUKEI04-HKNID          PIC  9(10).
      *        �ݸ����ֹ�
               05  TOUKEI04-HKNJANUM       PIC  X(08).
      *        �ܿͲ�²��ʬ
               05  TOUKEI04-HONKZKKBN      PIC  X(01).
      *        �����ʬ
               05  TOUKEI04-HOJOKBN        PIC  X(01).
      *        ��³��ʬ
               05  TOUKEI04-CONTKBN        PIC  X(01).
      *    �������
           03  TOUKEI04-KOHI-AREA.
      *        ���񣱡ݸ����ֹ�
               05  TOUKEI04-KOH1HKNNUM     PIC  X(03).
      *        ���񣱡ݸ���ɣ�
               05  TOUKEI04-KOH1ID         PIC  9(10).
      *        ���񣱡ݻ�ʧ��ʬ
               05  TOUKEI04-KOH1PAYKBN     PIC  X(02).
      *        ���񣲡ݸ����ֹ�
               05  TOUKEI04-KOH2HKNNUM     PIC  X(03).
      *        ���񣲡ݸ���ɣ�
               05  TOUKEI04-KOH2ID         PIC  9(10).
      *        ���񣲡ݻ�ʧ��ʬ
               05  TOUKEI04-KOH2PAYKBN     PIC  X(02).
      *        ���񣳡ݸ����ֹ�
               05  TOUKEI04-KOH3HKNNUM     PIC  X(03).
      *        ���񣳡ݸ���ɣ�
               05  TOUKEI04-KOH3ID         PIC  9(10).
      *        ���񣳡ݻ�ʧ��ʬ
               05  TOUKEI04-KOH3PAYKBN     PIC  X(02).
      *        ���񣴡ݸ����ֹ�
               05  TOUKEI04-KOH4HKNNUM     PIC  X(03).
      *        ���񣴡ݸ���ɣ�
               05  TOUKEI04-KOH4ID         PIC  9(10).
      *        ���񣴡ݻ�ʧ��ʬ
               05  TOUKEI04-KOH4PAYKBN     PIC  X(02).
      *    �������ô��������
           03  TOUKEI04-KOHFTN-TABLE  REDEFINES  TOUKEI04-KOHI-AREA
                                                  OCCURS  4.
      *        ����ݸ����ֹ�
               05  TOUKEI04-KOH-HKNNUM     PIC  X(03).
      *        ����ݸ���ɣ�
               05  TOUKEI04-KOH-ID         PIC  9(10).
      *        ����ݻ�ʧ��ʬ
               05  TOUKEI04-KOH-PAYKBN     PIC  X(02).
      *    ϫ�Ҿ���
           03  TOUKEI04-HKN.
      *        ϫ�ҡ�ϫ���ݸ���ʬ
               05  TOUKEI04-RSIHKNKBN      PIC  X(01).
      *        ϫ�ҡݼ����������ʬ
               05  TOUKEI04-JIBAISEIKBN    PIC  X(01).
      *    ����°������
           03  TOUKEI04-PTINF-AREA.
      *        ͹���ֹ�
               05  TOUKEI04-HOME-POST       PIC  X(07).
      *        �����������Υ�����
               05  TOUKEI04-LPUBCD          PIC  X(05).
      *        ����
               05  TOUKEI04-HOME-ADRS       PIC  X(400).
      *        �����ֹ�
               05  TOUKEI04-HOME-TEL        PIC  X(15).
