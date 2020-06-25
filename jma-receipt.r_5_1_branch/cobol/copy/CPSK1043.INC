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
      *         �����ƥ�����ޥ���     (CPSK1043.INC)   *
      *         �ʣ���������                            *
      *         ��׾Ȳ�ǽ����                        *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  06/01/20  ���Ųʽ��ɽ���ɲ�
      *  04.03.00    NACL-¿��Ǽ  08/08/20  �쥻�ץȥ֥�ӥ塼��ʬ�ɲ�
      *  04.07.00    NACL-¿��Ǽ  12/11/21  �쥻�ץȥץ�ӥ塼�쥤�������ɲ�
      ************************************************************
       01  SYS-1043-REC.
           03  SYS-1043-KEY.
               05  SYS-1043-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1043-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1043-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1043-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1043-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1043-TBL.
      *���ʤ�ɽ�����ʬ
               05  SYS-1043-SRYKBN-FLG  PIC  X(01).
      *���Ųʤν��ɽ��
               05  SYS-1043-SRYKA-FLG   PIC  X(01).
      *�쥻�ץȥ֥�ӥ塼
               05  SYS-1043-RESPRVFLG   PIC  X(01).
      *�쥻�ץȥ֥�ӥ塼���쥤������
               05  SYS-1043-PRVLAYOUT   PIC  X(01).
      *ͽ��
      ******** 05  SYS-1043-YOBI        PIC  X(497).
               05  SYS-1043-YOBI        PIC  X(496).
      *ü���ɣ�
           03  SYS-1043-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1043-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1043-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1043-UPYMD           PIC  X(08).
      *��������
           03  SYS-1043-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1043-UPD-KEY.
               05  SYS-1043-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1043-UPD-KANRICD     PIC X(04).
               05  SYS-1043-UPD-KBNCD       PIC X(08).
               05  SYS-1043-UPD-STYUKYMD    PIC X(08).
               05  SYS-1043-UPD-EDYUKYMD    PIC X(08).