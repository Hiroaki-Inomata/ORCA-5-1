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
      *    HAORI���N�G�X�g�@���@���ܗ��ꊇ�ύX��� (��̉�)
      *                                     COPY (CPMDCXMLAV3REQ5.INC)
      *         �f�Ís�ד��e�i���@�j
      ******************************************************************
       01  XML-MEDICALAV3REQ5.
           03  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
           03  MDCREQ-OBJECT           PIC X(36).
           03  MDCREQ-MODE             PIC S9(9)   BINARY.
           03  MDCREQ-RECORDNAME       PIC X(128).
           03  MDCREQ-MEDICALREQ.
      *       ���N�G�X�g�ԍ�
               05  MDCREQ-REQUEST-NUMBER    PIC X(2).
      *        UID�i�d�q�J���e�j
               05  MDCREQ-KARTE-UID         PIC X(36).
      *        UID�i�I���J�j
               05  MDCREQ-ORCA-UID          PIC X(36).
      *
      *        ���Ҕԍ�
               05  MDCREQ-PATIENTID         PIC X(20).
      *        �f�ÔN��
               05  MDCREQ-PERFORM-YM        PIC X(07).
      *
      *���N�G�X�g���O�Q
      *        ���@���ܗ�
               05  MDCREQ-MEDICAL-CHOZAIINF-G.
      *             �f�É�
                    09  MDCREQ-CHZ-DEP-CODE        PIC X(2).
      *             �ی��g�����ԍ�
                    09  MDCREQ-CHZ-COMBINATION     PIC X(4).
      *
      *             �ܔԍ�
                    09  MDCREQ-CHZ-ZAINUM          PIC X(8).
      *
      *             �f�Î�ʋ敪
                    09  MDCREQ-CHZ-CLASS           PIC X(3).
      *             �f�Î�ʖ���
                    09  MDCREQ-CHZ-CLASS-NAME      PIC X(40).
      *             �Z���
                    09  MDCREQ-CHZ-DAY-AREA.
      *                 �Z�藚��
                        11  MDCREQ-CHZ-RRK-G.
      *                 ��
                            13  MDCREQ-CHZ-CLASS-NUMBER  PIC X(03)
                                                     OCCURS   31.
      *             �����ē��@�̎Z��
                    09  MDCREQ-CHZ-REDAY-G.
                        11  MDCREQ-CHZ-REDAY-OCC    OCCURS   31.
      *                     �����ē��@��
                            13  MDCREQ-CHZ-REDAY        PIC X(02).
      *                     ��
                            13  MDCREQ-CHZ-RECLASS-NUMBER  PIC X(03).
      *
                    09  MDCREQ-CHZ-INFO-G.
                        11  MDCREQ-CHZ-INFO    OCCURS   5.
      *                     �f�ÃR�[�h
                            13  MDCREQ-CHZ-CODE        PIC X(9).
      *                     �f�Ö���
                            13  MDCREQ-CHZ-NAME        PIC X(80).
      *                     �����敪
                            13  MDCREQ-CHZ-AUTOKBN     PIC X(01).
      *