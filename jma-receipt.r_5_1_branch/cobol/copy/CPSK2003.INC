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
      *****************************************************************
      *         �����ƥ�����ޥ���     (CPSK2003.INC)   
      *         �ʣ���������                           
      *         �쥻�ץȺ����ؼ��ʰ�������꣱��        
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01   MCC-ƣ��      02.02.26  �����ե�������ѹ��ˤ�꽤��
      *  01.00.02   MCC-ƣ��      02/06/17  ϫ�ҡ������դ˹��ܤ��ѹ�
      *
      *  01.01.01   NACL-ƣ��     02/09/24  ������ʬ��ɽ������ɲ�
      *****************************************************************
       01  SYS-2003-REC.
           03  SYS-2003-KEY.
               05  SYS-2003-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-2003-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-2003-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-2003-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-2003-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-2003-TBL.
      *�쥻�ץȼ���̾��
               05  SYS-2003-SYURUINAME  PIC  X(40).
      *������ʧ����
               05  SYS-2003-TEISYUTUSAKI
                                        PIC  X(01).
      *�쥻�ץȼ���
               05  SYS-2003-SYUBETU-G.
                   07  SYS-2003-SYUBETU PIC  X(04)  OCCURS 10.
      *������ʬ
               05  SYS-2003-NYUGAIKBN   PIC  X(01).
      *ɽ����
               05  SYS-2003-DSPSEQ      PIC  9(03).
      *ͽ��
           03  SYS-2003-YOBI            PIC  X(415).
      *ü���ɣ�
           03  SYS-2003-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-2003-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-2003-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-2003-UPYMD           PIC  X(08).
      *��������
           03  SYS-2003-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-2003-UPD-KEY.
               05  SYS-2003-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2003-UPD-KANRICD     PIC X(04).
               05  SYS-2003-UPD-KBNCD       PIC X(08).
               05  SYS-2003-UPD-STYUKYMD    PIC X(08).
               05  SYS-2003-UPD-EDYUKYMD    PIC X(08).