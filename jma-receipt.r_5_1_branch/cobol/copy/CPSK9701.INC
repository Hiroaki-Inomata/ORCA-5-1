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
       01  SYS-9701-REC.
           03  SYS-9701-KEY.
               05  SYS-9701-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-9701-KANRICD     PIC  X(04).
      *��ʬ�����ɡ�9701:P02 , 9702:U02 , 9703:K03 )
               05  SYS-9701-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-9701-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-9701-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-9701-TBL.
      *������Ģɼ̾
               05  SYS-9701-SYORINM        PIC  X(30).
      *������̾
               05  SYS-9701-SHELL          PIC  X(100).
      *�ץ��̾
               05  SYS-9701-PRTNM          PIC  X(16).
      *�¹Բ���
               05  SYS-9701-EXECUTE-SW     PIC  X(01).
      *�¹Է���
               05  SYS-9701-EXECUTE-YTPE   PIC  X(01).
      *ͽ��
           03  SYS-9701-YOBI            PIC  X(352).
      *ü���ɣ�
           03  SYS-9701-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-9701-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-9701-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-9701-UPYMD           PIC  X(08).
      *��������
           03  SYS-9701-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-9701-UPD-KEY.
               05  SYS-9701-UPD-HOSPNUM     PIC 9(2).
               05  SYS-9701-UPD-KANRICD     PIC X(04).
               05  SYS-9701-UPD-KBNCD       PIC X(08).
               05  SYS-9701-UPD-STYUKYMD    PIC X(08).
               05  SYS-9701-UPD-EDYUKYMD    PIC X(08).