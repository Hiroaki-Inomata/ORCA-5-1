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
      *    ���������\�i���������j
      *
      *  �쐬���t    ��Ǝ�  �L�q
      *  08/04/27    ��|    �V�K�쐬(�������ґΉ�)
      ********************************************************************
      *  �R�s�[��C������
      * Maj/Min/Rev  �C����  ���t        ���e
      *  01.00.01    �g��    11/06/17    �H�������̌��𑝂₷
      *  01.00.02    ���    19/02/04    �C�������R�����g�폜
      ********************************************************************
        01  SKT4615.
      *    �f�ÔN��
            03  SKT4615-SRYYM             PIC X(16).
      *    �����N����
            03  SKT4615-SEIYMD            PIC X(22).
      *    �Z��
            03  SKT4615-ADRS              PIC X(100).
      *    ��Ë@�֖�
            03  SKT4615-HOSPNAME          PIC X(100).
      *    �J�ݎҖ�
            03  SKT4615-KAISETUNAME       PIC X(40).
      *    ��Ë@�փR�[�h
            03  SKT4615-HOSPCD            PIC X(20).
      *    �ی��Җ�
            03  SKT4615-HKNJANAME         PIC X(20).
      *    �ی��Ҕԍ�
            03  SKT4615-HKNJANUM          PIC X(12).
      *    �ݑ��f�E�݈㑍
            03  SKT4615-ZAI1           PIC X(02).
            03  SKT4615-ZAI2           PIC X(02).
      *    �y�[�W�ԍ�
            03  SKT4615-NO                PIC X(04).
      *    �V�l�����e�[�u��
            03  SKT4615-SEIKYU-TBL  OCCURS  2 TIMES.
      *    ���O�e�[�u��
              05  SKT4615-NYUGAI-TBL  OCCURS  2 TIMES.
      *    ����
                07  SKT4615-KENSU         PIC X(05).
      *    �f�Î�����
                07  SKT4615-NISSU         PIC X(05).
      *    �_��
                07  SKT4615-TENSU         PIC X(10).
      *    �ꕔ���S��
                07  SKT4615-ITBFTN        PIC X(10).
      *    �H���×{�F����
              05  SKT4615-S-KENSU         PIC X(05).
      *    �H���×{�F����
              05  SKT4615-S-NISSU         PIC X(06).
      *    �H���×{�F���z
              05  SKT4615-S-KINGK         PIC X(10).
      *    �H���×{�F�W�����S�z
              05  SKT4615-S-FTNGK         PIC X(10).
      *    ������e�[�u��
            03  SKT4615-KOUHI-TBL  OCCURS  4 TIMES.
      *    ���
              05  SKT4615-HBTNUM          PIC X(02).
              05  SKT4615-KOHNAME         PIC X(10).
      *    ���O�e�[�u��
              05  SKT4615-KNYUGAI-TBL  OCCURS  2 TIMES.
      *    ����
                07  SKT4615-KKENSU        PIC X(05).
      *    �f�Î�����
                07  SKT4615-KNISSU        PIC X(05).
      *    �_��
                07  SKT4615-KTENSU        PIC X(10).
      *    �ꕔ���S��
                07  SKT4615-KITBFTN       PIC X(10).
      *    �H���×{�F����
              05  SKT4615-S-KKENSU        PIC X(05).
      *    �H���×{�F����
              05  SKT4615-S-KNISSU        PIC X(06).
      *    �H���×{�F���z
              05  SKT4615-S-KKINGK        PIC X(10).
      *    �H���×{�F�W�����S�z
              05  SKT4615-S-KFTNGK        PIC X(10).