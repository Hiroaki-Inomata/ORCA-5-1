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
      *  �v���O�����C������
      * Maj/Min/Rev  �C����       ���t      ���e
      * 01.00.00     @@@@-�c��    11/04/04  �V�K�쐬
      ********************************************************************
       01  HC17.
      *    �V�X�e�����t
           03  HC17-SYSYMD              PIC X(22).
      *    �f�Ó��t
           03  HC17-SRYYMD              PIC X(22).
      *    �Ő�
           03  HC17-PAGE                PIC X(03).
      *    ���o��
           03  HC17-MIDASI              PIC X(10).
      *    �W���u���b�Z�[�W
           03  HC17-JOBMSG              PIC X(60).
      *    ���O�敪
           03  HC17-NYUGAIKBN           PIC X(04).
      *    �f�É�
           03  HC17-SRYKA               PIC X(20).
      *    ����e�[�u��
           03  HC17-TJIHI-G             OCCURS 10 TIMES.
      *    �����
               05  HC17-TJIHI           PIC X(08).
      *    �A�ԍ��e�[�u��
           03  HC17-RENNUM-G            OCCURS 21 TIMES.
      *    �A�ԍ�
               05  HC17-RENNUM          PIC X(07).
      *    ���Ҕԍ��e�[�u��
           03  HC17-PTNUM-G             OCCURS 21 TIMES.
      *    ���Ҕԍ��i�P�O���ȉ��j
               05  HC17-PTNUM           PIC X(10).
      *    ���Ҕԍ��i�P�P���ȏ�j
               05  HC17-PTNUM2          PIC X(20).
      *    ���Ҏ����e�[�u��
           03  HC17-NAME-G              OCCURS 21 TIMES.
      *    ���Ҏ���
               05  HC17-NAME            PIC X(18).
      *    �ی��_���e�[�u��
           03  HC17-HKNTEN-G            OCCURS 21 TIMES.
      *    �ی��_��
               05  HC17-HKNTEN          PIC X(07).
      *    �W�v�敪�e�[�u��
           03  HC17-MONEY-G             OCCURS 20 TIMES.
      *    ���������z�e�[�u���i�W�v�敪�ʁj
               05  HC17-MONEY-OCC       OCCURS 21 TIMES.
      *    ���������z�i�W�v�敪�ʁj
                   07  HC17-MONEY       PIC X(07).
      *    ��Î��v�e�[�u��
           03  HC17-SYUEKI-G            OCCURS 21 TIMES.
      *    ��Î��v
               05  HC17-SYUEKI          PIC X(08).
      *    ���������z�e�[�u��
           03  HC17-SEIKYU-G            OCCURS 21 TIMES.
      *    ���������z
               05  HC17-SEIKYU          PIC X(08).
      *    �����̎��z�e�[�u��
           03  HC17-RYOSYU-G            OCCURS 21 TIMES.
      *    �����̎��z
               05  HC17-RYOSYU          PIC X(08).
      *    �ی��_���\���v�i�V���j
           03  HC17-G-HKNTEN            PIC X(07).
      *    �ی��_���\���v�i�W���j
           03  HC17-GG-HKNTEN           PIC X(08).
      *    ��Î��v�\���v�i�W���j
           03  HC17-G-SYUEKI            PIC X(08).
      *    ��Î��v�\���v�i�X���j
           03  HC17-GG-SYUEKI           PIC X(09).
      *    �W�v�敪�e�[�u���\���v
           03  HC17-G-MONEY-TBL         OCCURS 20 TIMES.
      *    ���������z�\���v�i�W�v�敪�ʁj�i�V���j
               05  HC17-G-MONEY         PIC X(07).
      *    �W�v�敪�e�[�u���\���v
           03  HC17-GG-MONEY-TBL        OCCURS 20 TIMES.
      *    ���������z�\���v�i�W�v�敪�ʁj�i�W���j
               05  HC17-GG-MONEY        PIC X(08).
      *    ���������z�\���v�i�W���j
           03  HC17-G-SEIKYU            PIC X(08).
      *    ���������z�\���v�i�X���j
           03  HC17-GG-SEIKYU           PIC X(09).
      *    �����̎��z�\���v�i�W���j
           03  HC17-G-RYOSYU            PIC X(08).
      *    �����̎��z�\���v�i�X���j
           03  HC17-GG-RYOSYU           PIC X(09).