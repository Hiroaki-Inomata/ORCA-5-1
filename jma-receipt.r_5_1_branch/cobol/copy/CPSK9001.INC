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
      *         �����ƥ�����ޥ���     (CPSK9001.INC)            *
      *         �ʣ���������                                     *
      *         �ạ̃��ɣ��̿���������                         *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-9001-REC.
           03  SYS-9001-KEY.
               05  SYS-9001-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-9001-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-9001-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-9001-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-9001-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
      *�����ѥ��ɥ쥹
           03  SYS-9001-ADDRESS         PIC  X(40).
      *�����ѥݡ��ȡʼ����ѡ�
           03  SYS-9001-PORT1           PIC  X(05).
      *�����ѥݡ��ȡʿ��Ź԰��ѡ�
           03  SYS-9001-PORT2           PIC  X(05).
      *��������襳����
           03  SYS-9001-MEMO            PIC  X(40).
      *ͽ��
           03  SYS-9001-YOBI            PIC  X(410).
      *ü���ɣ�
           03  SYS-9001-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-9001-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-9001-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-9001-UPYMD           PIC  X(08).
      *��������
           03  SYS-9001-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-9001-UPD-KEY.
               05  SYS-9001-UPD-HOSPNUM     PIC 9(2).
               05  SYS-9001-UPD-KANRICD     PIC X(04).
               05  SYS-9001-UPD-KBNCD       PIC X(08).
               05  SYS-9001-UPD-STYUKYMD    PIC X(08).
               05  SYS-9001-UPD-EDYUKYMD    PIC X(08).