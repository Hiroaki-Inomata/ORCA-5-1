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
      *         �����ƥ�����ޥ���     (CPSK3004.INC)   *
      *         �ʣ���������                            *
      *         ��ñ�Ȼ��Ⱦ��󥳡���                    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    04.08.31  �ѥ�᥿���̡��ץ��̾
      *                                     ���ɲ�
      *
      *  04.09.01    NACL-ƣ��    16/11/18  �ģ������б�
      ************************************************************
       01  SYS-3004-REC.
           03  SYS-3004-KEY.
               05  SYS-3004-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-3004-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-3004-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-3004-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-3004-EDYUKYMD    PIC  X(08).
      *������ID
           03  SYS-3004-SHELLID         PIC  X(020).
      *����̾��
           03  SYS-3004-SHORIMSG        PIC  X(040).
      *����ե饰
           03  SYS-3004-CHK.
               05  SYS-3004-CHKFLG      PIC  X(01)
                                        OCCURS   4.
      *�ѥ�᥿����
           03  SYS-3004-PARAKBN         PIC  X(01).
      *�ץ��̾
           03  SYS-3004-PRTNM           PIC  X(16).
      *��ݥ��ȥ�̾
           03  SYS-3004-NAME            PIC  X(128).
      *ͽ��
           03  SYS-3004-YOBI            PIC  X(290).
      *ü���ɣ�
           03  SYS-3004-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-3004-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-3004-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-3004-UPYMD           PIC  X(08).
      *��������
           03  SYS-3004-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-3004-UPD-KEY.
               05  SYS-3004-UPD-HOSPNUM     PIC 9(2).
               05  SYS-3004-UPD-KANRICD     PIC X(04).
               05  SYS-3004-UPD-KBNCD       PIC X(08).
               05  SYS-3004-UPD-STYUKYMD    PIC X(08).
               05  SYS-3004-UPD-EDYUKYMD    PIC X(08).