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
      *****************************************************************
      *  ͽ������ޥ�����       (CPYYKEXAMPLE.INC)
      *****************************************************************
      *  ��������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *01  YYKEXAMPLE-REC.
           03  YYKEXAMPLE-KEY.
      *���ŵ��أɣ�
               05  YYKEXAMPLE-HOSPNUM       PIC 9(2).
      *Ϣ��
               05  YYKEXAMPLE-RENNUM        PIC 9(03).
      *ɽ����ʬ
           03  YYKEXAMPLE-HYOKBN            PIC 9(01).
      *����
           03  YYKEXAMPLE-COMMENT           PIC X(100).
      *ü���ɣ�
           03  YYKEXAMPLE-TERMID            PIC X(16).
      *���ڡ��졼���ɣ�
           03  YYKEXAMPLE-OPID              PIC X(16).
      *����ǯ����
           03  YYKEXAMPLE-CREYMD            PIC X(08).
      *����ǯ����
           03  YYKEXAMPLE-UPYMD             PIC X(08).
      *��������
           03  YYKEXAMPLE-UPHMS             PIC X(06).