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
      * 01.00.00     @@@@-�c��    11/09/29  �W�v�Ώۓ���͈͎w��ɕύX
      ********************************************************************
       01  D600.
      *    �W���u���b�Z�[�W
           03  D600-JOBMSG            PIC X(60).
      *    �Ő�
           03  D600-PAGE              PIC X(03).
      *    �W�v�Ώۓ��t
           03  D600-SYSYMD            PIC X(22).
      *    ���o��
           03  D600-MIDASI            PIC X(10).
      *    �����ԋ���
           03  D600-NYUHEN-YMD        PIC X(50).
      *    ���O�敪
           03  D600-NYUGAIKBN         PIC X(04).
      *    �f�É�
           03  D600-SRYKA             PIC X(20).
      *    ���@���i���ږ��́j
           03  D600-NYUINRYO          PIC X(08).
      *    �×{�蓖�āi���ږ��́j
           03  D600-TRYO              PIC X(08).
      *    �A�ԍ��e�[�u��
           03  D600-RENNUM-TBL        OCCURS 21 TIMES.
      *    �A��
               05  D600-RENNUM        PIC X(07).
      *    ���Ҕԍ��e�[�u��
           03  D600-PTNUM-TBL         OCCURS 21 TIMES.
      *    ���Ҕԍ��i�P�O���ȉ��j
               05  D600-PTNUM         PIC X(10).
      *    ���Ҕԍ��i�P�P���ȏ�j
               05  D600-PTNUM2        PIC X(20).
      *    ���Ҏ����e�[�u��
           03  D600-NAME-TBL          OCCURS 21 TIMES.
      *    ���Ҏ���
               05  D600-NAME          PIC X(18).
      *    ���ʃe�[�u��
           03  D600-SEX-TBL           OCCURS  21 TIMES.
      *    ����
               05  D600-SEX           PIC X(02).
      *    �N��e�[�u��
           03  D600-AGE-TBL           OCCURS  21 TIMES.
      *    �N��
               05  D600-AGE           PIC X(03).
      *    �{�l�Ƒ��e�[�u��
           03  D600-HKNKBN-TBL        OCCURS  21 TIMES.
      *    �{�l�Ƒ�
               05  D600-HKNKBN        PIC X(02).
      *    �ی���ރe�[�u��
           03  D600-HKNS-TBL          OCCURS  21 TIMES.
      *    �ی����
               05  D600-HKNS          PIC X(08).
      *    �ی��_���e�[�u��
           03  D600-HKNTEN-TBL        OCCURS  21 TIMES.
      *    �ی��_��
               05  D600-HKNTEN        PIC X(07).
      *    �f�Ë敪�e�[�u��
           03  D600-TEN-HK-TBL        OCCURS  16 TIMES.
      *    �ی��_���e�[�u���i�f�Ë敪�ʁj
               05  D600-TEN-TBL       OCCURS  21 TIMES.
      *    �ی��_���i�f�Ë敪�ʁj
                   07  D600-TEN       PIC X(07).
      *    �ی����t�z�e�[�u��
           03  D600-HKNFTN-TBL        OCCURS  21 TIMES.
      *    �ی����t�z
               05  D600-HKNFTN        PIC X(08).
      *    ����S�z�e�[�u��
           03  D600-JIHIFTN-TBL       OCCURS  21 TIMES.
      *    ����S�z
               05  D600-JIHIFTN       PIC X(08).
      *    ���������z�e�[�u��
           03  D600-SEIKYU-TBL        OCCURS  21 TIMES.
      *    ���������z
               05  D600-SEIKYU        PIC X(08).
      *    �����̎��z�e�[�u��
           03  D600-RYOSYU-TBL        OCCURS  21 TIMES.
      *    �����̎��z
               05  D600-RYOSYU        PIC X(08).
      *    �ی��_���\���v�i�V���j
           03  D600-G-HKNTEN          PIC X(07).
      *    �ی��_���\���v�i�W���j
           03  D600-GG-HKNTEN         PIC X(08).
      *    �ی��_���\���v�e�[�u���i�f�Ë敪�ʁj
           03  D600-G-TEN-HK-TBL      OCCURS  16 TIMES.
      *    �ی��_���\���v�i�f�Ë敪�ʁj�i�V���j
               05  D600-G-TEN         PIC X(07).
      *    �ی��_���\���v�e�[�u���i�f�Ë敪�ʁj
           03  D600-GG-TEN-HK-TBL     OCCURS  16 TIMES.
      *    �ی��_���\���v�i�f�Ë敪�ʁj�i�W���j
               05  D600-GG-TEN        PIC X(08).
      *    �ی����t�z�\���v�i�W���j
           03  D600-G-HKNFTN          PIC X(08).
      *    �ی����t�z�\���v�i�X���j
           03  D600-GG-HKNFTN         PIC X(09).
      *    ����S�z�\���v�i�W���j
           03  D600-G-JIHIFTN         PIC X(08).
      *    ����S�z�\���v�i�X���j
           03  D600-GG-JIHIFTN        PIC X(09).
      *    ���������z�\���v�i�W���j
           03  D600-G-SEIKYU          PIC X(08).
      *    ���������z�\���v�i�X���j
           03  D600-GG-SEIKYU         PIC X(09).
      *    �����̎��z�\���v�i�W���j
           03  D600-G-RYOSYU          PIC X(08).
      *    �����̎��z�\���v�i�X���j
           03  D600-GG-RYOSYU         PIC X(09).