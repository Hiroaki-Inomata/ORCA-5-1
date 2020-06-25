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
      *-------------------------------------------------*
      *         �����ƥ�����ޥ���     (CPSK2004.INC)   *
      *         �ʣ���������                            *
      *         �쥻�ץȺ����ؼ��ʥ쥻�ץȼ��̡�        *
      *-------------------------------------------------*
       01  SYS-2004-REC.
           03  SYS-2004-KEY.
               05  SYS-2004-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-2004-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-2004-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-2004-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-2004-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-2004-TBL.
      *�쥻�ץȼ���̾��
               05  SYS-2004-SYUBETUNAME PIC  X(40).
      *�쥻�ץȽ��Ͻ�
               05  SYS-2004-PRTJYUNI    PIC  X(02).
      *ͽ��
           03  SYS-2004-YOBI            PIC  X(458).
      *ü���ɣ�
           03  SYS-2004-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-2004-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-2004-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-2004-UPYMD           PIC  X(08).
      *��������
           03  SYS-2004-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-2004-UPD-KEY.
               05  SYS-2004-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2004-UPD-KANRICD     PIC X(04).
               05  SYS-2004-UPD-KBNCD       PIC X(08).
               05  SYS-2004-UPD-STYUKYMD    PIC X(08).
               05  SYS-2004-UPD-EDYUKYMD    PIC X(08).