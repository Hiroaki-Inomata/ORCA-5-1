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
      *    �쥻�ץȺ��������ޥ��������ѥѥ�᥿
      *                                    COPY (CPORCSRECEUPD.INC)
      *                                   ���ѥץ������ (ORCSRECEUPD)
      *    �������
      *      ������ʬ  �������ԣɣĤΤ�  ����
      *      ���ԣɣ�
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCSRECEUPDAREA.
      *    ������ʬ    �������ԣɣĤΤߤǽ���
      *                ����
           03  LNK-UPD-SYORI           PIC X(01).
      *    ����������  ����������ʬ���顼
           03  LNK-UPD-RC              PIC X(01).
      *    ���ŵ��أɣ�
           03  LNK-UPD-HOSPNUM         PIC 9(2).
      *    ���ԣɣ�
           03  LNK-UPD-PTID            PIC 9(10).
      *    ������ʬ
           03  LNK-UPD-NYUGAIKBN       PIC X(01).
      *    ���Ų�
           03  LNK-UPD-SRYKA           PIC X(02).
      *    ����ǯ��
           03  LNK-UPD-SRYYM           PIC X(06).
      *    �ݸ���ʬ
           03  LNK-UPD-HKNKBN          PIC X(01).
      *    �ݸ���ʬ��
           03  LNK-UPD-HKNKBN2         PIC X(01).
      *    ü���ɣ�
           03  LNK-UPD-TERMID          PIC  X(16).
      *    ���ڡ��졼���ɣ�
           03  LNK-UPD-OPID            PIC  X(16).
