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
      *         �����ƥ�����ޥ���     (CPSK9700.INC)   *
      *         �ʣ���������                            *
      *         �桼���ץ�����൯ư                 *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-9700-REC.
           03  SYS-9700-KEY.
               05  SYS-9700-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-9700-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-9700-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-9700-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-9700-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-9700-TBL.
               05  SYS-9700-TBL-OCC     OCCURS   25.
      *�ؼ����̣ɣ�
                   07  SYS-9700-GYOUMUID         PIC  X(10).
      *�ؼ�����������
                   07  SYS-9700-GYOUMU-KANRICD   PIC  X(04).
      *�ؼ����̽��ɽ��
                   07  SYS-9700-GYOUMU-FLG       PIC  X(01).
      *�ؼ����ɽ��
                   07  SYS-9700-GYOUMU-CNT       PIC  9(02).
      *ͽ��
                   07  SYS-9700-YOBI             PIC  X(03).
      *ü���ɣ�
           03  SYS-9700-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-9700-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-9700-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-9700-UPYMD           PIC  X(08).
      *��������
           03  SYS-9700-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-9700-UPD-KEY.
               05  SYS-9700-UPD-HOSPNUM     PIC 9(2).
               05  SYS-9700-UPD-KANRICD     PIC X(04).
               05  SYS-9700-UPD-KBNCD       PIC X(08).
               05  SYS-9700-UPD-STYUKYMD    PIC X(08).
               05  SYS-9700-UPD-EDYUKYMD    PIC X(08).