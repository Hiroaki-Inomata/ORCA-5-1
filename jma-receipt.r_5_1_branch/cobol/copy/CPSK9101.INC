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
      *         �����ƥ�����ޥ���     (CPSK9101.INC)            *
      *         �ʣ���������                                     *
      *         ���ž���Ϣ�ȴ����Ѿ���                           *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-9101-REC.
           03  SYS-9101-KEY.
               05  SYS-9101-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-9101-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-9101-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-9101-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-9101-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-9101-KANRITBL.
      *Ϣ�ȴ����ѥǡ�������
               05  SYS-9101-COOPBASE    PIC X(01).
      *Ϣ�ȴ����ѥǡ����ե�����ǥ��쥯�ȥ�
               05  SYS-9101-FILE-DIR    PIC X(200).
      *����̾�쥳���ɵ�Ͽ
               05  SYS-9101-SYREC       PIC X(01).
      *�����ݸ�ʬ����
               05  SYS-9101-JIHI        PIC X(01).
      *�����ֹ浭Ͽ
               05  SYS-9101-PTNUM       PIC X(01).
      *����̾̾������Ͽ
               05  SYS-9101-SYBYO       PIC X(01).
      *���Ź԰١������ʥ쥳���ɤ�̾�ε�Ͽ
               05  SYS-9101-NMREC       PIC X(01).
      *Ϣ�ȴ�������������ʬ
               05  SYS-9101-COOPBASE2   PIC X(01).
      *ͽ��
               05  SYS-9101-YOBI        PIC X(293).
      *         05  SYS-9101-YOBI        PIC X(294).
      *         05  SYS-9101-YOBI        PIC X(295).
      *         05  SYS-9101-YOBI        PIC X(296).
      *ü���ɣ�
           03  SYS-9101-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-9101-OPID        PIC  X(16).
      *����ǯ����
           03  SYS-9101-CREYMD      PIC  X(08).
      *����ǯ����
           03  SYS-9101-UPYMD       PIC  X(08).
      *��������
           03  SYS-9101-UPHMS       PIC  X(06).