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
      *         �����ƥ�����ޥ���     (CPSK5004.INC)   *
      *         �ʣ���������                            *
      *         ������������ޥ���                      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-5010-REC.
           03  SYS-5010-KEY.
               05  SYS-5010-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-5010-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-5010-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-5010-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-5010-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-5010-TBL.
               05  SYS-5010-SEIKYU-DAY-VAL.
      *����������
                   07  SYS-5010-SEIKYU-DAY1 PIC  9(02).
      *����������
                   07  SYS-5010-SEIKYU-DAY2 PIC  9(02).
      *����������
                   07  SYS-5010-SEIKYU-DAY3 PIC  9(02).
               05  SYS-5010-SEIKYU-DAY-R    REDEFINES
                   SYS-5010-SEIKYU-DAY-VAL.
                   07  SYS-5010-SEIKYU-DAY-OCC   OCCURS  3.
                       09  SYS-5010-SEIKYU-DAY
                                            PIC  9(02).
      *        ͽ��
               05  SYS-5010-YOBI1           PIC  X(10).
      *        �����ʬ
               05  SYS-5010-NYUKINKBN       PIC  X(01).
      *        �ƽ�����ʬ
               05  SYS-5010-SAISYORIKBN     PIC  X(01).
      *        ���������ʬ
               05  SYS-5010-NYUKINPRTKBN    PIC  X(01).
      *        �����ʬ
               05  SYS-5010-DELKBN          PIC  X(01).
      *        �����ȯ����ˡ��ʬ
               05  SYS-5010-SKYSUMKBN       PIC  X(01).
      *        ��ư���ܶ�ʬ
               05  SYS-5010-CHGWINDOWKBN    PIC  X(01).
      *        ����������ʬ
               05  SYS-5010-SKYPRTKBN       PIC  X(01).
      *        ���ٽ������ʬ
               05  SYS-5010-SRYPRTKBN       PIC  X(01).
      *        ���Ը��̾��󻲾ȶ�ʬ
               05  SYS-5010-PTINFREFKBN     PIC  X(01).
      *        ���Ͻ��ʬ
               05  SYS-5010-PRTJYUN         PIC  X(02).
      *        �ౡ���Խ��׶�ʬ
               05  SYS-5010-TAIINGRPKBN     PIC  X(01).
      *        ͽ��
               05  SYS-5010-YOBI2           PIC  X(472).
      *ü���ɣ�
           03  SYS-5010-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-5010-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-5010-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-5010-UPYMD           PIC  X(08).
      *��������
           03  SYS-5010-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-5010-UPD-KEY.
               05  SYS-5010-UPD-HOSPNUM     PIC 9(2).
               05  SYS-5010-UPD-KANRICD     PIC X(04).
               05  SYS-5010-UPD-KBNCD       PIC X(08).
               05  SYS-5010-UPD-STYUKYMD    PIC X(08).
               05  SYS-5010-UPD-EDYUKYMD    PIC X(08).
