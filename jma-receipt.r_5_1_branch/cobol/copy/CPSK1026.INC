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
      *         �����ƥ�����ޥ���     (CPSK1026.INC)   *
      *         �ʣ���������                            *
      *         ���Ȼ�ͳ                                *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-1026-REC.
           03  SYS-1026-KEY.
               05  SYS-1026-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1026-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1026-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1026-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1026-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1026-TBL.
      *�±����Ȼ�ͳ
               05  SYS-1026-DISCOUNT-BODY    PIC  X(40).
      *���Ψ�ʶ�ʬ��
               05  SYS-1026-DISCOUNT-KBN     PIC  X(02).
      *����о�
               05  SYS-1026-DISCOUNT-OBJ     PIC  X(01).
      *ͽ��
           03  SYS-1026-YOBI            PIC  X(457).
      *ü���ɣ�
           03  SYS-1026-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1026-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1026-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1026-UPYMD           PIC  X(08).
      *��������
           03  SYS-1026-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1026-UPD-KEY.
               05  SYS-1026-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1026-UPD-KANRICD     PIC X(04).
               05  SYS-1026-UPD-KBNCD       PIC X(08).
               05  SYS-1026-UPD-STYUKYMD    PIC X(08).
               05  SYS-1026-UPD-EDYUKYMD    PIC X(08).