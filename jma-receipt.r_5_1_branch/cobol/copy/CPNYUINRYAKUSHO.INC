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
      *    �����쥻�ץȵ���ά��         COPY (CPNYUINRYAKUSHO.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  NYURYAKUSHO-REC.
           03  NYURYAKUSHO-KEY.
      *�����������ť�����
               05  NYURYAKUSHO-SRYCD   PIC  X(09).
      *ͭ������ǯ����
               05  NYURYAKUSHO-YUKOSTYMD    PIC  X(08).
      *ͭ����λǯ����
               05  NYURYAKUSHO-YUKOEDYMD    PIC  X(08).
      *ά��
           03  NYURYAKUSHO-RYAKUSHO     PIC  X(20).
      *̾��
           03  NYURYAKUSHO-NAME         PIC  X(200).
      *ü���ɣ�
           03  NYURYAKUSHO-TERMID       PIC  X(16).
      *���ڡ��졼���ɣ�
           03  NYURYAKUSHO-OPID         PIC  X(16).
      *����ǯ����
           03  NYURYAKUSHO-CREYMD       PIC  X(08).
      *����ǯ����
           03  NYURYAKUSHO-UPYMD        PIC  X(08).
      *��������
           03  NYURYAKUSHO-UPHMS        PIC  X(06).
      *������
           03  NYURYAKUSHO-UP-KEY.
               05  NYURYAKUSHO-UPSRYCD     PIC  X(09).
               05  NYURYAKUSHO-UPYUKOSTYMD PIC  X(08).
               05  NYURYAKUSHO-UPYUKOEDYMD PIC  X(08).
      *