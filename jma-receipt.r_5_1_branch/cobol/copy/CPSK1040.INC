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
      *         �����ƥ�����ޥ���     (CPSK1040.INC)   *
      *         �ʣ���������                            *
      *         �ǡ��������å���ǽ����                  *
      *         ������ô���ֹ桦������ֹ����������    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-1040-REC.
           03  SYS-1040-KEY.
               05  SYS-1040-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1040-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1040-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1040-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1040-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1040-TBL.
      *��ô���ֹ�ȴ�������å�
               05  SYS-1040-FTNJANUMCHK PIC  X(01).
      *������ֹ�ȴ�������å�
               05  SYS-1040-JKYSNUMCHK  PIC  X(01).
      *��ô���ֹ�Ⱦ��ʸ������
               05  SYS-1040-FTNJHANKAKUCHK
                                        PIC  X(01).
      *��ô���ֹ�����ʸ������
               05  SYS-1040-FTNJZENKAKUCHK
                                        PIC  X(01).
      *������ֹ�Ⱦ��ʸ������
               05  SYS-1040-JKYSHANKAKUCHK
                                        PIC  X(01).
      *������ֹ�����ʸ������
               05  SYS-1040-JKYSZENKAKUCHK
                                        PIC  X(01).
      *��ô���ֹ�����ʳ���ʸ������
               05  SYS-1040-FTNJNUMERICCHK
                                        PIC  X(01).
      *������ֹ�����ʳ���ʸ������
               05  SYS-1040-JKYSNUMERICCHK
                                        PIC  X(01).
      *��ô���ֹ��������å�
               05  SYS-1040-FTNJKETACHK
                                        PIC  X(01).
      *��ô���ֹ����Ǿ�
               05  SYS-1040-FTNJKETAMIN
                                        PIC  X(02).
      *��ô���ֹ�������
               05  SYS-1040-FTNJKETAMAX
                                        PIC  X(02).
      *������ֹ��������å�
               05  SYS-1040-JKYSKETACHK
                                        PIC  X(01).
      *������ֹ����Ǿ�
               05  SYS-1040-JKYSKETAMIN
                                        PIC  X(02).
      *������ֹ�������
               05  SYS-1040-JKYSKETAMAX
                                        PIC  X(02).
      *ͽ��
               05  SYS-1040-YOBI        PIC  X(482).
      *ü���ɣ�
           03  SYS-1040-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1040-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1040-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1040-UPYMD           PIC  X(08).
      *��������
           03  SYS-1040-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1040-UPD-KEY.
               05  SYS-1040-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1040-UPD-KANRICD     PIC X(04).
               05  SYS-1040-UPD-KBNCD       PIC X(08).
               05  SYS-1040-UPD-STYUKYMD    PIC X(08).
               05  SYS-1040-UPD-EDYUKYMD    PIC X(08).