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
      *         �����ƥ�����ޥ���     (CPSK1039.INC)   *
      *         �ʣ���������                            *
      *         ��Ǽ��ǽ����                            *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-1039-REC.
           03  SYS-1039-KEY.
               05  SYS-1039-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1039-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1039-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1039-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1039-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1039-TBL.
      *���������ʬ
               05  SYS-1039-NYKNKNRKBN  PIC  X(01).
      *�������μ���ȯ�Զ�ʬ�ʳ����
               05  SYS-1039-SKYPRTGKBN  PIC  X(01).
      *�������μ���ȯ�Զ�ʬ��������
               05  SYS-1039-SKYPRTNKBN  PIC  X(01).
      *���׻���
               05  SYS-1039-STIME.
                   07  SYS-1039-STIME-HH   PIC  X(02).
                   07  SYS-1039-STIME-MM   PIC  X(02).
      *��������
               05  SYS-1039-KARITIME.
                   07  SYS-1039-KARITIME-OCC1          OCCURS  2.
                       09  SYS-1039-KARIDAY    PIC X   OCCURS  7.
                       09  SYS-1039-KARITIME-OCC2      OCCURS  2.
                           11  SYS-1039-KARISTTIME.
                               13  SYS-1039-KARIST-HH  PIC X(02).
                               13  SYS-1039-KARIST-MM  PIC X(02).
                           11  SYS-1039-KARIEDTIME.
                               13  SYS-1039-KARIED-HH  PIC X(02).
                               13  SYS-1039-KARIED-MM  PIC X(02).
      *���������ٽ�ȯ�Զ�ʬ�ʳ����
               05  SYS-1039-SRYPRTGKBN  PIC  X(01).
      *���������ٽ�ȯ�Զ�ʬ��������
               05  SYS-1039-SRYPRTNKBN  PIC  X(01).
      *�����������������μ���ȯ�Զ�ʬ��������
               05  SYS-1039-S06SKYPRTNKBN
                                        PIC  X(01).
      *�����������������μ���ȯ�Զ�ʬ�ʳ����
               05  SYS-1039-S06SKYPRTGKBN
                                        PIC  X(01).
      *���������̳���ʬȯ����ˡ
               05  SYS-1039-S06SKYSUMKBN
                                        PIC  X(01).
      *����������̳����������μ���ȯ����ˡ
               05  SYS-1039-S02SKYSAIKBN
                                        PIC  X(01).
      *����������̳����������μ���̤��������ʬ
               05  SYS-1039-S02SKYMISYUPRTKBN
                                        PIC  X(01).
      *�����ǧ���̳����������μ���̤��������ʬ
               05  SYS-1039-S03SKYMISYUPRTKBN
                                        PIC  X(01).
      *����������̷����������μ���̤��������ʬ
               05  SYS-1039-TUKISKYMISYUPRTKBN
                                        PIC  X(01).
      *��ʧ������������̻�ʧ������ȯ�Զ�ʬ
               05  SYS-1039-S04SHIHARAIKBN      PIC X(01).
      *��ʧ�������������������ʬ
               05  SYS-1039-S04NYUGAIKBN        PIC X(01).
      *��ʧ������������̴��ֻ���
               05  SYS-1039-S04SRYKBN           PIC X(01).
      *��ʧ������������̴��ֶ�ʬ
               05  SYS-1039-S04KIKANKBN         PIC X(01).
      *��ʧ��������������������Ƥΰ���
               05  SYS-1039-S04DSPKBN           PIC X(01).
      *��ʧ������������̴��ֽ�λ��λ�ʧ��
               05  SYS-1039-S04SYORIKBN         PIC X(01).
      *��ʧ������������̴��ֳ���������ʬ�λ�ʧ��
               05  SYS-1039-S04SUMKBN           PIC X(01).
      *
      *������������¤ӽ�
               05  SYS-1039-S02SORTKBN          PIC X(01).
      *���Ʒ׻���
               05  SYS-1039-IKTSORTKBN          PIC X(01).
      *���Ʒ׻���ˡ
               05  SYS-1039-IKTKEISANKBN        PIC X(01).
      *����������ǯ������ʬ
               05  SYS-1039-IKTSRYYMKBN         PIC X(01).
      *���������ȯ����ˡ
               05  SYS-1039-TUKIGRPKBN          PIC X(01).
      *��������񽸷׶�ʬ
               05  SYS-1039-TUKISUMKBN          PIC X(01).
      *�����̤���⽸�״��ֶ�ʬ��������
               05  SYS-1039-MISYUKIKANKBN       PIC X(01).
      *�Ʒ׻������ԥ����å�
               05  SYS-1039-SAIKEISANSKYCHK     PIC X(01).
      *��ʧ������������̼����������ٽ�
               05  SYS-1039-S04JIHIPRTKBN       PIC X(01).
      *����������̥����ꥹ�Ƚ��ɽ��
               05  SYS-1039-S02MODE             PIC X(01).
      *����������̥����ꥹ�Ⱥ�����
               05  SYS-1039-S02LIMIT            PIC 9(03).
      *ͽ��
               05  SYS-1039-YOBI                PIC  X(418).
      *ü���ɣ�
           03  SYS-1039-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1039-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1039-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1039-UPYMD           PIC  X(08).
      *��������
           03  SYS-1039-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1039-UPD-KEY.
               05  SYS-1039-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1039-UPD-KANRICD     PIC X(04).
               05  SYS-1039-UPD-KBNCD       PIC X(08).
               05  SYS-1039-UPD-STYUKYMD    PIC X(08).
               05  SYS-1039-UPD-EDYUKYMD    PIC X(08).
