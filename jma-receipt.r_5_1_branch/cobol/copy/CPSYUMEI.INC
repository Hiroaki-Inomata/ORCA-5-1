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
      *    ��Ǽ���٥ޥ���    COPY (CPSYUMEI.INC)       *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  SYUMEI-REC.
           03  SMEI-KEY.
      *���ŵ��أɣ�
               05  SMEI-HOSPNUM          PIC 9(2).
      *������ʬ
               05  SMEI-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  SMEI-PTID            PIC  9(10).
      *��ɼ�ֹ�
               05  SMEI-DENPNUM         PIC  9(07).
      *��ɼ�ֹ����
               05  SMEI-DENPEDANUM      PIC  9(02).
      *����Ϣ��
           03  SMEI-NYUKINRENNUM        PIC  9(03).
      *���Ų�
           03  SMEI-SRYKA               PIC  X(02).
      *���پ��ֶ�ʬ
           03  SMEI-MEISAIJOUTAIKBN     PIC  X(01).
      *����ȯ����
           03  SMEI-SKYPRINTYMD         PIC  X(08).
      *�����ȯ����
           03  SMEI-SKYREPRINTYMD       PIC  X(08).
      *�μ���ȯ����
           03  SMEI-RYOSYUPRINTYMD      PIC  X(08).
      *�μ����ȯ����
           03  SMEI-RYOSYUREPRINTYMD    PIC  X(08).
      *������
           03  SMEI-SKYMONEY            PIC S9(07).
      *���ֶ��ʬ
           03  SMEI-NYUHEN-KBN          PIC  X(01).
      *���ֶ��
           03  SMEI-NYUHEN-MONEY        PIC S9(07).
      *���ֶ���
           03  SMEI-NYUHEN-YMD          PIC  X(08).
      *������ˡ
           03  SMEI-NYUKIN-HOHO         PIC  X(02).
      *���ֶ�ʬ
           03  SMEI-JOUTAIKBN           PIC  X(01).
      *���󥭡�
           03  SMEI-DAILYKEY            PIC  X(12).
      *��Ǽ�����ֹ�
           03  SMEI-SYURRKNUM           PIC 9(2).
      *��Ǽ������ֹ�
           03  SMEI-SYUEDANUM           PIC 9(2).
      *��Ǽ���򹹿���ʬ
           03  SMEI-SYURRKUPDKBN        PIC X(01).
      *ü���ɣ�
           03  SMEI-TERMID              PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SMEI-OPID                PIC  X(16).
      *����ǯ����
           03  SMEI-CREYMD              PIC  X(08).
      *��������
           03  SMEI-CREHMS              PIC  X(06).
      *����ǯ����
           03  SMEI-UPYMD               PIC  X(08).
      *��������
           03  SMEI-UPHMS               PIC  X(06).
      *
      *����ǯ�����ʸ����ѡ�
           03  SMEI-UPNYUHEN-YMD        PIC  X(08).
      *�������󥭡�
           03  SMEI-STDAILYKEY          PIC  X(12).
      *��λ���󥭡�
           03  SMEI-EDDAILYKEY          PIC  X(12).
      *���Ϻ���ǯ����
           03  SMEI-STCREYMD            PIC  X(8).
      *��λ����ǯ����
           03  SMEI-EDCREYMD            PIC  X(8).
