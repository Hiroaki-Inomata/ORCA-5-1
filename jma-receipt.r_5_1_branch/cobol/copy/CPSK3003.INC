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
      *         �����ƥ�����ޥ���     (CPSK3003.INC)   *
      *         �ʣ���������                            *
      *         ��ñ�Ȼ��Ⱦ��󥳡���                    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    02.08.30  �����ؼ����ɲ�
      *  01.00.02    NACL-ƣ��    03.05.06  ��������ģ��б��ѥե饰
      *                                     ���ɲ�
      *  01.00.03    NACL-ƣ��    05.02.14  ��������ģ��б��ѥե饰
      *                                    ��̤���ѡˤ�ɸ����ݥ쥻������
      *                                     ɽ�����ؼ����ѹ�
      *
      *  04.02.01    NACL-ƣ��    08/03/31  ʿ������ǯ��������б�
      *                                     ����Ϣ���ɲ�
      ************************************************************
       01  SYS-3003-REC.
           03  SYS-3003-KEY.
               05  SYS-3003-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-3003-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-3003-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-3003-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-3003-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-3003-TBL.
      *�ˣţ�
               05  SYS-3003-TBL-KEY     PIC  X(08).
      *�ʣϣ£ɣ�
               05  SYS-3003-JOBID       PIC  9(07).
      *���롼��
               05  SYS-3003-TBL-GROUP   PIC  X(14).
      *�����ؼ�������ΤȤ��Τ߻��ѡ�
      *ɸ���������ɽ�����ؼ�
               05  SYS-3003-PRTKBN1     PIC  X(01).
      *ɸ���������ɽ�����ؼ�
               05  SYS-3003-PRTKBN2     PIC  X(01).
      *ɸ����ݥ쥻������ɽ�����ؼ�
               05  SYS-3003-PRTKBN3     PIC  X(01).
      *ɸ�๭��Ϣ������ɽ�����ؼ�
               05  SYS-3003-PRTKBN4     PIC  X(01).
      *ɸ�๭��Ϣ��쥻������ɽ�����ؼ�
               05  SYS-3003-PRTKBN5     PIC  X(01).
      *ͽ��
           03  SYS-3003-YOBI            PIC  X(466).
      *ü���ɣ�
           03  SYS-3003-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-3003-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-3003-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-3003-UPYMD           PIC  X(08).
      *��������
           03  SYS-3003-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-3003-UPD-KEY.
               05  SYS-3003-UPD-HOSPNUM     PIC 9(2).
               05  SYS-3003-UPD-KANRICD     PIC X(04).
               05  SYS-3003-UPD-KBNCD       PIC X(08).
               05  SYS-3003-UPD-STYUKYMD    PIC X(08).
               05  SYS-3003-UPD-EDYUKYMD    PIC X(08).
