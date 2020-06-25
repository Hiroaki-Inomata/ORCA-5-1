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
      *         �����ƥ�����ޥ���     (CPSK0043.INC)   *
      *         �ʣ���������                            *
      *         �������ץѥ�᡼�������ɸ���󶡡�      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-0043-REC.
           03  SYS-0043-KEY.
               05  SYS-0043-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-0043-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-0043-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-0043-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-0043-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-0043-TBL.
      *Ģɼ̾
               05  SYS-0043-HCNAME      PIC  X(160).
      *�ץ������̾
               05  SYS-0043-PRGNAME     PIC  X(24).
      *�ѥ�᥿
               05  SYS-0043-PARA-TBL.
                   07  SYS-0043-PARA-G      OCCURS  10.
                       09  SYS-0043-PARAHYOJI
                                        PIC X(20).
                       09  SYS-0043-PARA    PIC X(10).
      *ɬ�ܶ�ʬ��SPACE or 1:ɬ��2:Ǥ�ա�
               05  SYS-0043-HISSU-KBN   OCCURS  10
                                        PIC X(01).
      *ͽ��
               05  SYS-0043-YOBI        PIC  X(06).
      *ü���ɣ�
           03  SYS-0043-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-0043-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-0043-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-0043-UPYMD           PIC  X(08).
      *��������
           03  SYS-0043-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-0043-UPD-KEY.
               05  SYS-0043-UPD-HOSPNUM     PIC 9(2).
               05  SYS-0043-UPD-KANRICD     PIC X(04).
               05  SYS-0043-UPD-KBNCD       PIC X(08).
               05  SYS-0043-UPD-STYUKYMD    PIC X(08).
               05  SYS-0043-UPD-EDYUKYMD    PIC X(08).