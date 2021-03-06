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
      *    ����������������������ѥ�᥿��COPY (CPORCBT120SUB.INC)
      *                                ���ѥץ������ (ORCBT120SUB.CBL)
      *    �������
      *      ���ŵ��أɣ�
      *      ���ԣɣ�
      *      ����ǯ����
      *      ���Ų�
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      ************************************************************
       01  ORCBT120SUB-AREA.
      *
      *���ŵ��أɣ�
           03  LNK-ORCBT120SUB-HOSPNUM                 PIC  9(02).
      *���ԣɣ�
           03  LNK-ORCBT120SUB-PTID                    PIC  9(10).
      *����ǯ����
           03  LNK-ORCBT120SUB-SANTEIYMD               PIC  X(08).
      *���Ųʡʣ��������ʤǸ��ꥻ�åȡ�
           03  LNK-ORCBT120SUB-SRYKA                   PIC  X(02).
      *���������ֹ�
           03  LNK-ORCBT120SUB-RRKNUM                  PIC  9(03).
      *�����ֵѹ���
           03  LNK-ORCBT120SUB-HENKYAKU-AREA.
      *��ե�����
               05  LNK-ORCBT120SUB-DRCD                PIC  X(05).
      *���ౡ��ʬ�ʣ����������������������ౡ�������������
               05  LNK-ORCBT120SUB-NYUIN-KBN           PIC  9(01).
      *�����ֹ�
               05  LNK-ORCBT120SUB-BTUNUM              PIC  X(02).
      *�¼��ֹ�
               05  LNK-ORCBT120SUB-BRMNUM              PIC  X(06).
      *���������ֹ�
               05  LNK-ORCBT120SUB-MAE-BTUNUM          PIC  X(02).
      *�����¼��ֹ�
               05  LNK-ORCBT120SUB-MAE-BRMNUM          PIC  X(06).
      *�����ʬ
               05  LNK-ORCBT120SUB-GAIHAKU-KBN         PIC  9(01).
      *������ʬ
               05  LNK-ORCBT120SUB-SKJ-KBN             PIC  9(01).
      *�ౡ��ͳ
               05  LNK-ORCBT120SUB-TAIINCD             PIC  X(02).
      *������
               05  LNK-ORCBT120SUB-NYUINKA             PIC  X(02).
      *�쥻��������
               05  LNK-ORCBT120SUB-RECENYUINKA         PIC  X(02).
      *����������
               05  LNK-ORCBT120SUB-MAE-NYUINKA         PIC  X(02).
      *�����쥻��������
               05  LNK-ORCBT120SUB-MAE-RECENYUINKA     PIC  X(02).
      *�����ݸ��ȹ礻�ֹ�
               05  LNK-ORCBT120SUB-NYU-HKNCOMBINUM     PIC  9(04).
      *��Ǽ�����ֵѥ��ꥢ
               05  LNK-ORCBT120SUB-SYU-AREA            OCCURS 10.
      *��ɼ�ֹ�
                   07  LNK-ORCBT120SUB-DENPNUM         PIC  9(07).
      *��ɼ���ֶ�ʬ
                   07  LNK-ORCBT120SUB-DENPJTIKBN      PIC  X(01).
      *��ɼ������ʬ
                   07  LNK-ORCBT120SUB-CREATEKBN       PIC  X(01).
      *�ݸ�����
                   07  LNK-ORCBT120SUB-HKNTEN          PIC  9(07).
      *�ݸ���ô���
                   07  LNK-ORCBT120SUB-HKNMONEY        PIC  9(07).
      *�ݸ�Ŭ�ѳ����
                   07  LNK-ORCBT120SUB-TGMONEY         PIC  9(07).
      *�ݸ�Ŭ�ѳ����ǳ�
                   07  LNK-ORCBT120SUB-TGMONEY-TAX     PIC  9(07).
      *������
                   07  LNK-ORCBT120SUB-JIHI            PIC  9(07).
      *�����ǳ�
                   07  LNK-ORCBT120SUB-JIHI-TAX        PIC  9(07).
      *������ô���
                   07  LNK-ORCBT120SUB-SKJ             PIC  9(07).
      *�����ǳ�
                   07  LNK-ORCBT120SUB-SKJ-TAX         PIC  9(07).
      *��������
                   07  LNK-ORCBT120SUB-RMSAGAKU        PIC  9(07).
      *���������ǳ�
                   07  LNK-ORCBT120SUB-RMSAGAKU-TAX    PIC  9(07).
      *������
                   07  LNK-ORCBT120SUB-SKYMONEY        PIC  9(07).
      *�쥻�ץȼ���
                   07  LNK-ORCBT120SUB-RECESYUBETU     PIC  X(04).
      *�ݸ��ȹ礻�ֹ�
                   07  LNK-ORCBT120SUB-HKNCOMBINUM     PIC  9(04).
      *�ݸ��ֹ�
                   07  LNK-ORCBT120SUB-HKNNUM          PIC  X(03).
      *�ݸ��ɣ�
                   07  LNK-ORCBT120SUB-HKNID           PIC  9(10).
      *�ݸ����ֹ�
                   07  LNK-ORCBT120SUB-HKNJANUM        PIC  X(08).
      *�ܿͲ�²��ʬ
                   07  LNK-ORCBT120SUB-HONKZKKBN       PIC  X(01).
      *�����ʬ
                   07  LNK-ORCBT120SUB-HOJOKBN         PIC  X(01).
      *��³��ʬ
                   07  LNK-ORCBT120SUB-CONTKBN         PIC  X(01).
      *�������ô����
                   07  LNK-ORCBT120SUB-KOHAREA.
      *���񣱡ݸ����ֹ�
                     09  LNK-ORCBT120SUB-KOH1HKNNUM    PIC  X(03).
      *���񣱡ݸ���ɣ�
                     09  LNK-ORCBT120SUB-KOH1ID        PIC  9(10).
      *���񣱡ݻ�ʧ��ʬ
                     09  LNK-ORCBT120SUB-KOH1PAYKBN    PIC  X(02).
      *���񣲡ݸ����ֹ�
                     09  LNK-ORCBT120SUB-KOH2HKNNUM    PIC  X(03).
      *���񣲡ݸ���ɣ�
                     09  LNK-ORCBT120SUB-KOH2ID        PIC  9(10).
      *���񣲡ݻ�ʧ��ʬ
                     09  LNK-ORCBT120SUB-KOH2PAYKBN    PIC  X(02).
      *���񣳡ݸ����ֹ�
                     09  LNK-ORCBT120SUB-KOH3HKNNUM    PIC  X(03).
      *���񣳡ݸ���ɣ�
                     09  LNK-ORCBT120SUB-KOH3ID        PIC  9(10).
      *���񣳡ݻ�ʧ��ʬ
                     09  LNK-ORCBT120SUB-KOH3PAYKBN    PIC  X(02).
      *���񣴡ݸ����ֹ�
                     09  LNK-ORCBT120SUB-KOH4HKNNUM    PIC  X(03).
      *���񣴡ݸ���ɣ�
                     09  LNK-ORCBT120SUB-KOH4ID        PIC  9(10).
      *���񣴡ݻ�ʧ��ʬ
                     09  LNK-ORCBT120SUB-KOH4PAYKBN    PIC  X(02).
      *�������ô��������
                   07  LNK-ORCBT120SUB-KOHFTN-TABLE  REDEFINES
                                        LNK-ORCBT120SUB-KOHAREA
                                                       OCCURS  4.
      *����ݸ����ֹ�
                     09  LNK-ORCBT120SUB-KOH-HKNNUM  PIC  X(03).
      *����ݸ���ɣ�
                     09  LNK-ORCBT120SUB-KOH-ID      PIC  9(10).
      *����ݻ�ʧ��ʬ
                     09  LNK-ORCBT120SUB-KOH-PAYKBN  PIC  X(02).
      *ϫ���ݸ���ʬ
                 07  LNK-ORCBT120SUB-RSIHKNKBN       PIC  X(01).
      *����������
                 07  LNK-ORCBT120SUB-JIBAIKBN        PIC  X(01).
      *�ҳ���ʬ�ʥ쥻�ץȼ��̼����ѡ�
                 07  LNK-ORCBT120SUB-SAIGAIKBN       PIC  X(01).
      *
      *���������ɡʣ������������顼���������Ը������顼���������Ųʥ��顼
      *            ����̤��������������������顼��
           03  LNK-ORCBT120SUB-RC                    PIC S9(9) BINARY.
