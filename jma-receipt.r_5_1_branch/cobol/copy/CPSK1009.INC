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
      *         �����ƥ�����ޥ���     (CPSK1009.INC)   *
      *         �ʣ���������                            *
      *         �����ֹ湽������                        *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-¿��Ǽ   01/11/27  ��ͳ������ʬ�ɲ�
      ************************************************************
       01  SYS-1009-REC.
           03  SYS-1009-KEY.
               05  SYS-1009-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1009-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1009-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1009-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1009-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1009-TBL.
      *�����ֹ湽����ʬ
               05  SYS-1009-PTNUMKSIKBN             PIC X(01).
      *��ͳ����
               05  SYS-1009-JIYKSI.
      *    ��ͳ������ʬ
                   07  SYS-1009-JIYKSIKBN           PIC X(01).
      *    ��ͳ�������
                   07  SYS-1009-JIYKSIKETA          PIC 9(02).
      *ɸ�๽����ʬ
               05  SYS-1009-HJNKSI.
      *    ɸ�๽����ʬ
                   07  SYS-1009-HJNKSIKBN           PIC X(01).
      *    ɸ�๽��ǯ��ʬ
                   07  SYS-1009-HJNKSINENKBN        PIC X(01).
      *    ɸ�๽��Ϣ�ֽ������ʬ
                   07  SYS-1009-HJNKSIRENNUMKBN     PIC X(01).
      *    ɸ�๽��Ϣ�ֹ���
                   07  SYS-1009-HJNKSIRENNUMKETA    PIC 9(02).
      *��ĥ������ʬ
               05  SYS-1009-KKCYKSI.
      *    ��ĥ������ʬ
                   07  SYS-1009-KKCKSIKBN           PIC X(01).
      *    ��ĥ���������
                   07  SYS-1009-KKCKSIMAEKETA       PIC 9(02).
      *    ��ĥ����Ϣ�ֹ���
                   07  SYS-1009-KKCKSIRENNUMKETA    PIC 9(02).
      *    ��ĥ��������
                   07  SYS-1009-KKCKSIATOKETA       PIC 9(02).
      *ǯ
               05  SYS-1009-NEN                 PIC X(04).
      *Ϣ�ֹ�
               05  SYS-1009-RENNUM              PIC 9(10).
      *���ԣɣ�
               05  SYS-1009-PTID                PIC 9(10).
      *ǯ���ؤ���
               05  SYS-1009-NENTUKI             PIC X(02).
      *ͽ��
      *********05                           PIC  X(461).
      *******  05                           PIC  X(460).
               05  SYS-1009-YOBI            PIC  X(458).
      *ü���ɣ�
               05  SYS-1009-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
               05  SYS-1009-OPID            PIC  X(16).
      *����ǯ����
               05  SYS-1009-CREYMD          PIC  X(08).
      *����ǯ����
               05  SYS-1009-UPYMD           PIC  X(08).
      *��������
               05  SYS-1009-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1009-UPD-KEY.
               05  SYS-1009-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1009-UPD-KANRICD     PIC X(04).
               05  SYS-1009-UPD-KBNCD       PIC X(08).
               05  SYS-1009-UPD-STYUKYMD    PIC X(08).
               05  SYS-1009-UPD-EDYUKYMD    PIC X(08).