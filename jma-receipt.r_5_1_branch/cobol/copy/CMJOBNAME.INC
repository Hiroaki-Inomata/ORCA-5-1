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
      *---------------------------------------------------------------*
      *    ������ɣ��б��ʣϣ�̾�Υơ��֥�   COPY (CMJOBNAME.INC)    *
      *---------------------------------------------------------------*
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-����     02/06/27  ���ԾȲ��ɲ�
      *  01.00.02    MCC-����     02/12/19  ��������ɲ�
      *  01.00.03    NACL-����    03/10/14  �������ԾȲ��ɲ�
      *  01.00.04    NACL-ƣ��    03/12/03  ���ץǡ��������ɲ�
      *
      *  04.05.01    NACL-ƣ��    09/10/01  ���ףãӣֽ����ɲ�
      *  05.00.01    NACL-��ƣ    18/02/02  Ϣ�ȴ����ɲ�
      *****************************************************************
       01  MNAME-DATA-REC.
           03  FILLER               PIC X(08)   VALUE   "RECEPT1".
           03  FILLER               PIC X(40)   VALUE   "�쥻�ץȺ���".
      *
           03  FILLER               PIC X(08)   VALUE   "RECEPT2".
           03  FILLER               PIC X(40)   VALUE   "�쥻�ץȰ���".
      *
           03  FILLER               PIC X(08)   VALUE   "ORCBSD1 ".
           03  FILLER               PIC X(40)   VALUE
                                    "�ǡ��������å�".
      *
           03  FILLER               PIC X(08)   VALUE   "ORCBSE1 ".
           03  FILLER               PIC X(40)   VALUE   "�ǡ�������".
      *
           03  FILLER               PIC X(08)   VALUE   "ORCBSF1 ".
           03  FILLER               PIC X(40)   VALUE   "���������Ѵ�".
      *
           03  FILLER               PIC X(08)   VALUE   "MSTMNT  ".
           03  FILLER               PIC X(40)   VALUE   "�ޥ�������".
      *
           03  FILLER               PIC X(08)   VALUE   "SOKATU".
           03  FILLER               PIC X(40)   VALUE
                                    "���硦���������".
      *
           03  FILLER               PIC X(08)   VALUE   "TENSU".
           03  FILLER               PIC X(40)   VALUE
                                    "�����ޥ�������".
      *
           03  FILLER               PIC X(08)   VALUE   "TENSU2".
           03  FILLER               PIC X(40)   VALUE
                                    "�в����ְ������ִ��ޥ�������".
      *
           03  FILLER               PIC X(08)   VALUE   "DAILY".
           03  FILLER               PIC X(40)   VALUE   "��������".
      *
           03  FILLER               PIC X(08)   VALUE   "MONTHLY".
           03  FILLER               PIC X(40)   VALUE   "�����".
      *
           03  FILLER               PIC X(08)   VALUE   "REPRINT".
           03  FILLER               PIC X(40)   VALUE
                                    "�ư�������".
           03  FILLER               PIC X(08)   VALUE   "ORCBQ01".
           03  FILLER               PIC X(40)   VALUE
                                    "���ԾȲ�".
           03  FILLER               PIC X(08)   VALUE   "TEIKI1".
           03  FILLER               PIC X(40)   VALUE
                                    "�������ݼ�Ǽ�ǡ�������".
           03  FILLER               PIC X(08)   VALUE   "TEIKI2".
           03  FILLER               PIC X(40)   VALUE
                                    "����������������".
           03  FILLER               PIC X(08)   VALUE   "ORCBI201".
           03  FILLER               PIC X(40)   VALUE
                                    "�������ԾȲ�".
      *
           03  FILLER               PIC X(08)   VALUE   "MONTHLY1".
           03  FILLER               PIC X(40)   VALUE
                                                "����ץǡ�������".
      *
           03  FILLER               PIC X(08)   VALUE   "CHKMST".
           03  FILLER               PIC X(40)   VALUE
                                                "�����å��ޥ�������".
      *
           03  FILLER               PIC X(08)   VALUE   "ORCBS01".
           03  FILLER               PIC X(40)   VALUE
                                                "��ʧ������".
      *
           03  FILLER               PIC X(08)   VALUE   "DAILY1".
           03  FILLER               PIC X(40)   VALUE
                                                "�������ץǡ�������".
      *
           03  FILLER               PIC X(08)   VALUE   "TEIKI3".
           03  FILLER               PIC X(40)   VALUE
                                    "��������������".
           03  FILLER               PIC X(08)   VALUE   "YKJMST".
           03  FILLER               PIC X(40)   VALUE
                                    "���޾���ޥ���".
           03  FILLER               PIC X(08)   VALUE   "ORHCML09".
           03  FILLER               PIC X(40)   VALUE
                                    "������̾����".
           03  FILLER               PIC X(08)   VALUE   "COLLECT".
           03  FILLER               PIC X(40)   VALUE
                                    "�ǡ�������".
      *
           03  FILLER               PIC X(08)   VALUE   "PUTCSV".
           03  FILLER               PIC X(40)   VALUE
                                    "���ףãӣֽ���".
      *
           03  FILLER               PIC X(08)   VALUE   "INFECTIO".
           03  FILLER               PIC X(40)   VALUE
                                    "�����ɥ����٥�".
      *
           03  FILLER               PIC X(08)   VALUE   "RENKEI1".
           03  FILLER               PIC X(40)   VALUE
                                    "Ϣ�ȴ���".
      *
           03  FILLER               PIC X(08)   VALUE   "MISRECE1".
           03  FILLER               PIC X(40)   VALUE
                                    "���ž����ѥ쥻��".
      *
           03  FILLER               PIC X(08)   VALUE   "MISJMAMR".
           03  FILLER               PIC X(40)   VALUE
                                    "���ž���ե����ޥå�".
      *
           03  FILLER               PIC X(08)   VALUE   "MIGRATE1".
           03  FILLER               PIC X(40)   VALUE
                                    "�ޥ����ǡ����ܹ�".
      *
       01  MNAME-DATA-REC-R         REDEFINES   MNAME-DATA-REC.
           03  MNAME-TBL-OCC        OCCURS      30  INDEXED TBL-IDX.
               05  MNAME-SHELLID    PIC X(08).
               05  MNAME-JOBNAME    PIC X(40).
      *
       01  MNAME-MAX                PIC 9(04)   VALUE   30.
      *
