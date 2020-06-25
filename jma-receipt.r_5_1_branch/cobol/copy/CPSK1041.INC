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
      *         �����ƥ�����ޥ���     (CPSK1041.INC)   *
      *         �ʣ���������                            *
      *         ������ˡ����                            *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  05/09/13  ��������ɲ�
      ************************************************************
       01  SYS-1041-REC.
           03  SYS-1041-KEY.
               05  SYS-1041-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1041-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1041-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1041-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1041-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1041-TBL.
      *������ˡ̾��
               05  SYS-1041-NYKN-NAME   PIC  X(30).
      *������ˡά��
               05  SYS-1041-NYKN-TANNAME
                                        PIC  X(10).
      *������ˡʬ���ʬ
               05  SYS-1041-NYKN-BUNRUI PIC  X(02).
      *�������
               05  SYS-1041-NYKN-JYOTAI PIC  X(01).
      *������֡��ౡ����
               05  SYS-1041-NYKN-JYOTAI-N
                                        PIC  X(01).
      *������֡���������
               05  SYS-1041-NYKN-JYOTAI-T
                                        PIC  X(01).
      *ͽ��
               05  SYS-1041-YOBI        PIC  X(455).
      *ü���ɣ�
           03  SYS-1041-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1041-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1041-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1041-UPYMD           PIC  X(08).
      *��������
           03  SYS-1041-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1041-UPD-KEY.
               05  SYS-1041-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1041-UPD-KANRICD     PIC X(04).
               05  SYS-1041-UPD-KBNCD       PIC X(08).
               05  SYS-1041-UPD-STYUKYMD    PIC X(08).
               05  SYS-1041-UPD-EDYUKYMD    PIC X(08).