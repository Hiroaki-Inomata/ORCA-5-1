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
      *         �����ƥ�����ޥ���     (CPSK1030.INC)            *
      *         �ʣ���������                                     *
      *         Ģɼ�Խ���ʬ                                     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     MCC-¿��Ǽ   02/03/15  ��ȯ�������ɲ�
      * 01.00.02     NACL-¿��Ǽ  04/12/10  ����ƣ���������ɲ�
      ************************************************************
       01  SYS-1030-REC.
           03  SYS-1030-KEY.
               05  SYS-1030-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1030-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1030-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1030-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1030-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1030-TBL.
      *��������ǣ�ɽ����ʬ
               05  SYS-1030-SYOHOGEPRTFLG
                                        PIC  X(01).
      *���������ȯ������ɽ����ʬ
               05  SYS-1030-SYOHODSPNAME
                                        PIC  X(20).
      *����ƣ��������
               05  SYS-1030-KARUTE3KBN
                                        PIC  X(01).
      *���޾���ݲ����ե�����ե����
               05  SYS-1030-GAZOFOLDER
                                        PIC  X(128).
      *��ȯ�����ʤؤ��ѹ��Ľ�̾
               05  SYS-1030-KOUHATUKA
                                        PIC  X(01).
      *ͽ��
           03  SYS-1030-YOBI            PIC  X(349).
      *ü���ɣ�
           03  SYS-1030-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1030-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1030-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1030-UPYMD           PIC  X(08).
      *��������
           03  SYS-1030-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1030-UPD-KEY.
               05  SYS-1030-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1030-UPD-KANRICD     PIC X(04).
               05  SYS-1030-UPD-KBNCD       PIC X(08).
               05  SYS-1030-UPD-STYUKYMD    PIC X(08).
               05  SYS-1030-UPD-EDYUKYMD    PIC X(08).