      *******************************************************************
      * Project code name "ORCA"
      * ����W�����Z�v�g�\�t�g�iJMA standard receipt software�j
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
        01  SC05.
      *�����N����
            03  SC05-SRYYM             PIC X(16).
      *�f�ÔN��
            03  SC05-SEIYMD            PIC X(22).
      *�ی��Җ�
            03  SC05-HKNJANAME         PIC X(26).
      *�ی��Ҕԍ�
            03  SC05-HKNJANUM          PIC X(6).
      *�_���\
            03  SC05-TENHYOKBN         PIC X(2)    OCCURS  4 TIMES.
      *��Ë@�փR�[�h
            03  SC05-HOSPCD            PIC X(7).
      *��Ë@�֏Z��
            03  SC05-ADRS              PIC X(100).
      *��Ë@�֖�
            03  SC05-HOSPNAME          PIC X(100).
      *�J�ݎҎ���
            03  SC05-KAISETU           PIC X(40).
      *�d�b�ԍ�
            03  SC05-TEL               PIC X(15).
      *�e�`�w
            03  SC05-FAX               PIC X(15).
      *�Ő�
            03  SC05-NO                PIC X(4).
      *�����e�[�u��
            03  SC05-SEIKYU81-TBL       OCCURS  7 TIMES.
                05  SC05-KENSU81       PIC X(5).
                05  SC05-TENSU81       PIC X(9).
            03  SC05-SEIKYU82-TBL       OCCURS  7 TIMES.
                05  SC05-KENSU82       PIC X(5).
                05  SC05-TENSU82       PIC X(9).
                05  SC05-ITBFTN82      PIC X(9).
            03  SC05-SEIKYU83-TBL       OCCURS  2 TIMES.
                05  SC05-KENSU83       PIC X(5).
                05  SC05-TENSU83       PIC X(9).
            03  SC05-SEIKYU84-TBL       OCCURS  2 TIMES.
                05  SC05-KENSU84       PIC X(5).
                05  SC05-TENSU84       PIC X(9).
                05  SC05-ITBFTN84      PIC X(9).
            03  SC05-SEIKYU85-TBL       OCCURS  7 TIMES.
                05  SC05-KENSU85       PIC X(5).
                05  SC05-TENSU85       PIC X(9).