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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  �v���O�����C������
      * Maj/Min/Rev  �C����       ���t      ���e
      *****************************************************************
      ******************************************************************
      *    HAORI���N�G�X�g�@�r������ꊇ������� (��̉�)
      *                                     COPY (CPMDCXMLV3REQ7.INC)
      ******************************************************************
       01  XML-MEDICALV3REQ7.
           03  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
           03  MDCREQ-OBJECT           PIC X(36).
           03  MDCREQ-MODE             PIC S9(9)   BINARY.
           03  MDCREQ-RECORDNAME       PIC X(128).
           03  MDCREQ-MEDICALREQ.
      *       ���N�G�X�g�ԍ�
              05  MDCREQ-REQUEST-NUMBER    PIC X(2).
      *       UID�i�d�q�J���e�j
              05  MDCREQ-KARTE-UID         PIC X(36).
      *       UID�i�I���J�j
              05  MDCREQ-ORCA-UID          PIC X(36).
      *       �폜���
              05  MDCREQ-DELETE-AREA.
      *           �폜���
                  07  MDCREQ-DELETE-CLASS          PIC X(03).
      *           UID�i�d�q�J���e�j
                  07  MDCREQ-DEL-KARTE-UID         PIC X(36).
      *           UID�i�I���J�j
                  07  MDCREQ-DEL-ORCA-UID          PIC X(64).
      *
      *       �m�F���b�Z�[�W�ԓ�
              05  MDCREQ-SELECT-ANSWER      PIC X(02).
      *
      *