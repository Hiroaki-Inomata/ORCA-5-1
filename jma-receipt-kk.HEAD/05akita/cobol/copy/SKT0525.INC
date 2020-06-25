      *******************************************************************
      * Project code name "ORCA"
      * ����W�����Z�v�g�\�t�g�iJMA standard receipt software�j
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
      ********************************************************************
      *    �����������i�H�c���j
      *
      *  �쐬���t    ��Ǝ�        �L�q
      *  08/04/27    ��|          �V�K�쐬(�������ґΉ�)
      ********************************************************************
      *  �R�s�[��C������
      * Maj/Min/Rev  �C����       ���t      ���e
      ********************************************************************
        01  SKT0525.
      *    �f�ÔN��
            03  SKT0525-SRYYM             PIC X(16).
      *    �����N����
            03  SKT0525-SEIYMD            PIC X(22).
      *    �_���\�敪
            03  SKT0525-TENHYOKBN-TBL     OCCURS  2.
                05  SKT0525-TENHYOKBN     PIC X(02).
      *    �ی��Җ�
            03  SKT0525-HKNJANAME         PIC X(30).
      *    �ی��Ҕԍ�
            03  SKT0525-HKNJANUM          PIC X(04).
      *    �Z��
            03  SKT0525-ADRS              PIC X(100).
      *    ��Ë@�֖�
            03  SKT0525-HOSPNAME          PIC X(100).
      *    �J�ݎҖ�
            03  SKT0525-KAISETUNAME       PIC X(40).
      *    ��Ë@�փR�[�h
            03  SKT0525-HOSPCD            PIC X(20).
      *    �ݑ��f�E�݈㑍
            03  SKT0525-ZAI1           PIC X(02).
            03  SKT0525-ZAI2           PIC X(02).
      *    �y�[�W�ԍ�
            03  SKT0525-NO                PIC X(04).
      *    �V�l�����e�[�u��
            03  SKT0525-SEIKYU-TBL  OCCURS  2 TIMES.
      *    ���O�e�[�u��
              05  SKT0525-NYUGAI-TBL  OCCURS  2 TIMES.
      *    ����
                07  SKT0525-KENSU         PIC X(05).
      *    �f�Î�����
                07  SKT0525-NISSU         PIC X(05).
      *    �_��
                07  SKT0525-TENSU         PIC X(10).
      *    �ꕔ���S��
                07  SKT0525-ITBFTN        PIC X(10).
      *    �H���×{�F����
              05  SKT0525-S-KENSU         PIC X(05).
      *    �H���×{�F����
              05  SKT0525-S-NISSU         PIC X(05).
      *    �H���×{�F���z
              05  SKT0525-S-KINGK         PIC X(10).
      *    �H���×{�F�W�����S�z
              05  SKT0525-S-FTNGK         PIC X(10).
      *    ������e�[�u��
            03  SKT0525-KOUHI-TBL.
      *    ���
              05  SKT0525-HBTNUM          PIC X(02).
              05  SKT0525-KOHNAME         PIC X(10).
      *    ���O�e�[�u��
              05  SKT0525-KNYUGAI-TBL  OCCURS  2 TIMES.
      *    ����
                07  SKT0525-KKENSU        PIC X(05).
      *    �f�Î�����
                07  SKT0525-KNISSU        PIC X(05).
      *    �_��
                07  SKT0525-KTENSU        PIC X(10).
      *    �ꕔ���S��
                07  SKT0525-KITBFTN       PIC X(10).
      *    �H���×{�F����
              05  SKT0525-S-KKENSU        PIC X(05).
      *    �H���×{�F����
              05  SKT0525-S-KNISSU        PIC X(05).
      *    �H���×{�F���z
              05  SKT0525-S-KKINGK        PIC X(10).
      *    �H���×{�F�W�����S�z
              05  SKT0525-S-KFTNGK        PIC X(10).