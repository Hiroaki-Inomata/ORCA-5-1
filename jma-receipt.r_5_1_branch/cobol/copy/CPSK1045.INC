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
      *         ͽ��ǽ����                            *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.05.00    NACL-¿��Ǽ  10/06/21  ͽ�����Ϣư�ɲ�
      *  04.07.00    NACL-¿��Ǽ  11/06/XX  �����ݸ��ȹ礻��ʬ�ɲ�
      ************************************************************
       01  SYS-1045-REC.
           03  SYS-1045-KEY.
               05  SYS-1045-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1045-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1045-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1045-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1045-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1045-TBL.
      *����ɥ�����ɽ��
               05  SYS-1045-INIT-DRCD   PIC  X(05).
      *ͽ������ν��ɽ��
               05  SYS-1045-YYKINIT-FLG PIC  X(01).
      *ͽ���Ⱥ�����
               05  SYS-1045-YYKMAXCNT   PIC  9(02).
      *ͽ��ɼȯ�Զ�ʬ
               05  SYS-1045-YYKHYO-KBN  PIC  X(01).
      *ͽ�󴵼��ֹ�Ϣư��ʬ
               05  SYS-1045-YYKPTNUM-KBN   PIC  X(01).
      *ͽ������ֹ�Ϣư��ʬ
               05  SYS-1045-YYKUKE-KBN  PIC  X(01).
      *�����ݸ��ȹ礻��ʬ
               05  SYS-1045-UKEHKNCHK-KBN    PIC  X(01).
      *ͽ��
               05  SYS-1045-YOBI        PIC  X(488).
      *ü���ɣ�
           03  SYS-1045-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1045-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1045-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1045-UPYMD           PIC  X(08).
      *��������
           03  SYS-1045-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1045-UPD-KEY.
               05  SYS-1045-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1045-UPD-KANRICD     PIC X(04).
               05  SYS-1045-UPD-KBNCD       PIC X(08).
               05  SYS-1045-UPD-STYUKYMD    PIC X(08).
               05  SYS-1045-UPD-EDYUKYMD    PIC X(08).