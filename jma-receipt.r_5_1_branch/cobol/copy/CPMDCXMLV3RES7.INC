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
      *****************************************************************
      *  �v���O�����C������
      * Maj/Min/Rev  �C����       ���t      ���e
      *****************************************************************
      ******************************************************************
      *    �`�o�h���X�|���X�r������ꊇ�������  (��̉�)
      *                                      COPY (CPMDCXMLV3RES7.INC)
      ******************************************************************
       01  XML-MEDICALV3RES7.
            03  MDCRES-CONTEXT             PIC S9(9)   BINARY.
            03  MDCRES-OBJECT              PIC X(36).
            03  MDCRES-MODE                PIC S9(9)   BINARY.
            03  MDCRES-RECORDNAME          PIC X(128).
            03  MDCRES-MEDICALRES.
      *        ���N�G�X�g�ԍ�
               05  MDCRES-REQUEST-NUMBER    PIC X(2).
      *        ���X�|���X�ԍ�
               05  MDCRES-RESPONSE-NUMBER   PIC X(2).
      *        UID�i�d�q�J���e�j
               05  MDCRES-KARTE-UID         PIC X(36).
      *        UID�i�I���J�j
               05  MDCRES-ORCA-UID          PIC X(36).
      *        ���{��
               05  MDCRES-INFORMATION-DATE  PIC X(10).
      *        ���{����
               05  MDCRES-INFORMATION-TIME  PIC X(8).
      *        �ԋp�R�[�h
               05  MDCRES-API-RESULT        PIC X(3).
      *        �ԋp���b�Z�[�W
               05  MDCRES-API-RESULT-MSG    PIC X(100).
      *
               05  MDCRES-RESKEY            PIC X(100).
      *
      *        �I��ԋp����
      ***      05  MDCRES-SELECT-FLAG       PIC X(05).
      *        �폜���
               05  MDCRES-DELETE-AREA.
      *            �폜���
                   07  MDCRES-DELETE-CLASS          PIC X(03).
      *            UID�i�d�q�J���e�j
                   07  MDCRES-DEL-KARTE-UID         PIC X(36).
      *            UID�i�I���J�j
                   07  MDCRES-DEL-ORCA-UID          PIC X(64).
      *
      *        �r������ꗗ
               05  MDCRES-LOCK-LIST-G.
                   07  MDCRES-LOCK-LIST-OCC     OCCURS   100.
      *                �J���eUID
                       11  MDCRES-LOCK-KARTE-UID       PIC X(36).
      *                �I���J�t�h�c�iORCA�j
                       11  MDCRES-LOCK-TERMID-UID      PIC X(64).
      *                �I�y�[���[�^ID
                       11  MDCRES-LOCK-OPID            PIC X(16).
      *                �Ɩ��h�c
                       11  MDCRES-LOCK-LDNAME          PIC X(20).
      *                ���Ҕԍ�
                       11  MDCRES-LOCK-PATIENTID       PIC X(20).
      *                ���Ҏ���
                       11  MDCRES-LOCK-NAME            PIC X(100).
      *                �r�����t
                       11  MDCRES-LOCK-DATE            PIC X(10).
      *                �r������
                       11  MDCRES-LOCK-TIME            PIC X(08).
      *
      *       �m�F���b�Z�[�W�R�[�h�i���M�R�[�h�j
              05  MDCRES-SELECT-CODE         PIC X(04).
      *       �m�F���b�Z�[�W���e
              05  MDCRES-SELECT-MSG          PIC X(100).
      *