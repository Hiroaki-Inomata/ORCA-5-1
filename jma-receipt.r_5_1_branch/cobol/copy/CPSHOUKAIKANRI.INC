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
      *    ���ԾȲ񸡺�����������    COPY  (CPSHOUKAIKANRI.INC)*
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  SHOUKAIKANRI-REC.
      *����
           03  SHOKANRI-KEY.
      *���ŵ����ֹ�
               05  SHOKANRI-HOSPNUM    PIC  9(02).
      *UID
               05  SHOKANRI-UID        PIC  X(36).
      *Ϣ��
           03  SHOKANRI-RENNUM         PIC  S9(05).
      *�������̾��
           03  SHOKANRI-NAME           PIC  X(100).
      *������ʬ
           03  SHOKANRI-NYUGAIKBN      PIC  X(01).
      *ɽ���ʬ
           03  SHOKANRI-TITLEKBN       PIC  X(01).
      *ü���ɣ�
           03  SHOKANRI-TERMID         PIC  X(32).
      *���ڡ��졼���ɣ�
           03  SHOKANRI-OPID           PIC  X(16).
      *����ǯ����
           03  SHOKANRI-CREYMD         PIC  X(08).
      *����ǯ����
           03  SHOKANRI-UPYMD          PIC  X(08).
      *��������
           03  SHOKANRI-UPHMS          PIC  X(06).
      *virtual����
           03  SHOKANRI-RENNUM-FIRST   PIC  9(05).
           03  SHOKANRI-RENNUM-LAST    PIC  9(05).