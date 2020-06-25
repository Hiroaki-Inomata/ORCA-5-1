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
      *         �����ƥ�����ޥ���     (CPSK1050.INC)   *
      *         �ʣ���������                            *
      *         ��������                              *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 04.07.00     NACL-¿��Ǽ  14/02/10  ���ϥ���������Կ��ɲ�
      ************************************************************
       01  SYS-1050-REC.
           03  SYS-1050-KEY.
               05  SYS-1050-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1050-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1050-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1050-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1050-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1050-TBL.
      *���ڥ졼���ɣ�
               05  SYS-1050-ICCARDRW    PIC  X(1).
      *
      *���ϥ���������Կ�������
               05  SYS-1050-GLINECNT    PIC  X(02).
      *���ϥ���������Կ�������
               05  SYS-1050-NLINECNT    PIC  X(02).
      *�ץ�󥿤ؤΥǡ�������ñ��
               05  SYS-1050-PRTCONF     PIC  X(01).
      *ͽ��
      **** 03  SYS-1050-YOBI            PIC  X(499).
           03  SYS-1050-YOBI            PIC  X(494).
      *ü���ɣ�
           03  SYS-1050-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1050-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1050-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1050-UPYMD           PIC  X(08).
      *��������
           03  SYS-1050-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1050-UPD-KEY.
               05  SYS-1050-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1050-UPD-KANRICD     PIC X(04).
               05  SYS-1050-UPD-KBNCD       PIC X(08).
               05  SYS-1050-UPD-STYUKYMD    PIC X(08).
               05  SYS-1050-UPD-EDYUKYMD    PIC X(08).