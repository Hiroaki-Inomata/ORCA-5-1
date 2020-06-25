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
      *         �����ƥ�����ޥ���     (CPSK3001.INC)   *
      *         ��3��������                            *
      *         �쥻�ץȺ����ؼ�������                  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     01/09/06  �ѥ�᥿�����ɲ�
      ************************************************************
       01  SYS-3001-REC.
           03  SYS-3001-KEY.
               05  SYS-3001-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-3001-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-3001-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-3001-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-3001-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-3001-TBL.
      *Ģɼ̾
               05  SYS-3001-HCNAME      PIC  X(160).
      *�ץ������̾
               05  SYS-3001-PRGNAME     PIC  X(24).
      *�ѥ�᥿
               05  SYS-3001-PARA-AREA.
                   07  SYS-3001-PARA-TBL.
                       09  SYS-3001-PARA-G  OCCURS  10.
                           11  SYS-3001-PARAHYOJI
                                        PIC X(20).
                           11  SYS-3001-PARA    PIC X(10).
      *ɬ�ܶ�ʬ��SPACE or 1:ɬ��2:Ǥ�ա�
                   07  SYS-3001-HISSU-KBN
                                        OCCURS  10
                                        PIC X(01).
      *ͽ��
               05  SYS-3001-YOBI        PIC  X(06).
      *ü���ɣ�
           03  SYS-3001-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-3001-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-3001-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-3001-UPYMD           PIC  X(08).
      *��������
           03  SYS-3001-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-3001-UPD-KEY.
               05  SYS-3001-UPD-HOSPNUM     PIC 9(2).
               05  SYS-3001-UPD-KANRICD     PIC X(04).
               05  SYS-3001-UPD-KBNCD       PIC X(08).
               05  SYS-3001-UPD-STYUKYMD    PIC X(08).
               05  SYS-3001-UPD-EDYUKYMD    PIC X(08).