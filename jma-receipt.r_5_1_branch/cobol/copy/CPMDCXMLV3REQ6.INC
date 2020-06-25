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
      *    HAORI���N�G�X�g�@�ی��ꊇ�ύX���͏�� (��̉�)
      *                                     COPY (CPMDCXMLV3REQ6.INC)
      ******************************************************************
       01  XML-MEDICALV3REQ6.
           03  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
           03  MDCREQ-OBJECT           PIC X(36).
           03  MDCREQ-MODE             PIC S9(9)   BINARY.
           03  MDCREQ-RECORDNAME       PIC X(128).
           03  MDCREQ-MEDICALREQ.
      *       ���N�G�X�g�ԍ�
              05  MDCREQ-REQUEST-NUMBER    PIC X(2).
      *       UID�i�d�q�J���e�j
              05  MDCREQ-KARTE-UID         PIC X(36).
      *       ����t
      *****   05  MDCREQ-BASE-DATE         PIC X(10).
      *       UID�i�I���J�j
              05  MDCREQ-ORCA-UID          PIC X(36).
      *
      *       ���Ҕԍ�
              05  MDCREQ-PATIENTID         PIC X(20).
      *       �f�ÔN��
              05  MDCREQ-PERFORM-YM        PIC X(07).
      *       �f�É�
              05  MDCREQ-APPOINT-DEP-CODE  PIC X(02).
      *H30.6
      *       ���O�敪�i�P�F���@�j
              05  MDCREQ-OUTPATIENT-CLASS  PIC X(1).
      *
      *       �ꊇ�ύX���e�i���N�G�X�g���O�Q�j
              05  MDCREQ-CHENGE-INFO.
      *           �ύX�O�@�ی��g�����ԍ�
                  07  MDCREQ-IN-COMBINATION-NUMBER    PIC X(04).
      *           �ύX��@�ی��g�����ԍ�
                  07  MDCREQ-OT-COMBINATION-NUMBER    PIC X(04).
      *           �ύX����
                  07  MDCREQ-START-DAY         PIC X(02).
                  07  MDCREQ-END-DAY           PIC X(02).
      *           ���[�ꊇ�X�V����
                  07  MDCREQ-INCOME-CHANGE     PIC X(03).
      *