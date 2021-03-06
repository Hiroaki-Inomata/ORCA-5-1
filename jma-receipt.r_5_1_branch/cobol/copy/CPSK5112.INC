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
      *         �����ƥ�����ޥ���     (CPSK5112.INC)   *
      *         �ʣ���������                            *
      *         ���Ŵѻ�ˡ������ش�����                *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-5112-REC.
           03  SYS-5112-KEY.
               05  SYS-5112-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-5112-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-5112-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-5112-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-5112-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-5112-TBL.
      *���ť�����
               05  SYS-5112-SRYCD       PIC  X(09).
      *���Ŵѻ�ˡ������ش�����̾��
               05  SYS-5112-KANSATUMEI
                                        PIC  X(60).
      *���Ŵѻ�ˡ������ش�����̾�Σ�
               05  SYS-5112-KANSATUMEI2
                                        PIC  X(30).
      *ͽ��
               05  SYS-5121-YOBI        PIC  X(401).
      *ü���ɣ�
           03  SYS-5112-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-5112-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-5112-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-5112-UPYMD           PIC  X(08).
      *��������
           03  SYS-5112-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-5112-UPD-KEY.
               05  SYS-5112-UPD-HOSPNUM     PIC 9(2).
               05  SYS-5112-UPD-KANRICD     PIC X(04).
               05  SYS-5112-UPD-KBNCD       PIC X(08).
               05  SYS-5112-UPD-STYUKYMD    PIC X(08).
               05  SYS-5112-UPD-EDYUKYMD    PIC X(08).
