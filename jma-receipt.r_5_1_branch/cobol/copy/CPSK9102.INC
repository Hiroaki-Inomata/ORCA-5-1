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
      *         �����ƥ�����ޥ���     (CPSK9102.INC)            *
      *         �ʣ���������                                     *
      *         �ţƥե�����ǡ���                               *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.01    NACL-ƣ��    15/08/24  ����Τߤο��šʰ����ݸ��Τ�
      *                                     �����šˤΣţƽ��ϲ����б�
      ************************************************************
       01  SYS-9102-REC.
           03  SYS-9102-KEY.
               05  SYS-9102-HOSPNUM    PIC 9(02).
      *����������
               05  SYS-9102-KANRICD    PIC X(04).
      *��ʬ������
               05  SYS-9102-KBNCD      PIC X(08).
      *ͭ������ǯ����
               05  SYS-9102-STYUKYMD   PIC X(08).
      *ͭ����λǯ����
               05  SYS-9102-EDYUKYMD   PIC X(08).
      *�����ơ��֥�
           03  SYS-9102-KANRITBL.
      *�ţƥե�����ǡ�������
               05  SYS-9102-EFOUT      PIC X(01).
      *�ţƥե�����ǡ����ե�����ǥ��쥯�ȥ�
               05  SYS-9102-FILE-DIR   PIC X(200).
      *�ǡ��������ֹ浭Ͽ
               05  SYS-9102-NUMBER     PIC X(01).
      *�ౡ�������ؼ�
               05  SYS-9102-SRYCD-TBL.
                   07  SYS-9102-SRYCD  PIC X(09)   OCCURS  2.
      *����ΤߤΥ쥻�ץȽ��ϲ���
               05  SYS-9102-KOH-RECEIPT
                                       PIC X(01).
      *ͽ��
               05  SYS-9102-YOBI       PIC X(279).
      *ü���ɣ�
           03  SYS-9102-TERMID         PIC X(16).
      *���ڡ��졼���ɣ�
           03  SYS-9102-OPID           PIC X(16).
      *����ǯ����
           03  SYS-9102-CREYMD         PIC X(08).
      *����ǯ����
           03  SYS-9102-UPYMD          PIC X(08).
      *��������
           03  SYS-9102-UPHMS          PIC X(06).