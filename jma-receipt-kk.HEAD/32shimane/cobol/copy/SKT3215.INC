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
      *    �������Ґ����� �i�������j     COPY (SKT3215.INC) 
      *
      *  �쐬���t    ��Ǝ�        �L�q
      *  08/05/02    �g��          �V�K�쐬(�������ґΉ�)
      ********************************************************************
      *  �R�s�[��C������
      * Maj/Min/Rev  �C����       ���t      ���e
      ********************************************************************
       01  SKT3215.
      *    �f�ÔN��
           03  SKT3215-SRYYM                    PIC X(16).
      *    �����N����
           03  SKT3215-SEIYMD                   PIC X(22).
      *    ��Ë@�֏Z��
           03  SKT3215-ADRS                     PIC X(100).
      *    ��Ë@�֖���
           03  SKT3215-HOSPNAME                 PIC X(100).
      *    �J�ݎҎ���
           03  SKT3215-KAISETUNAME              PIC X(40).
      *    �d�b�ԍ�
           03  SKT3215-TEL                      PIC X(15).
      *    �ی��Җ�
           03  SKT3215-HKNJANAME                PIC X(40).
      *    ��Ë@�փR�[�h
           03  SKT3215-HOSPCD                   PIC X(14).
      *    �݈㑍�E�ݑ��f
           03  SKT3215-ZAI1                     PIC X(02).
           03  SKT3215-ZAI2                     PIC X(02).
      *    �y�[�W�ԍ�
           03  SKT3215-NO                       PIC X(04).
      *    �����e�[�u��
           03  SKT3215-SEIKYU-TBL      OCCURS  2.
      *    ���O�e�[�u��
               05  SKT3215-NYUGAI-TBL      OCCURS  2.
      *    ����
                   07  SKT3215-KENSU            PIC X(05).
      *    �f�Î�����
                   07  SKT3215-NISSU            PIC X(05).
      *    �_��
                   07  SKT3215-TENSU            PIC X(09).
      *    �ꕔ���S��
                   07  SKT3215-ITBFTN           PIC X(09).
      *    ����
                   07  SKT3215-TYOKI            PIC X(05).
      *    �H������
               05  SKT3215-S-KENSU              PIC X(05).
      *    �H������
               05  SKT3215-S-NISSU              PIC X(05).
      *    ���z
               05  SKT3215-S-KINGK              PIC X(09).
      *    �W�����S�z
               05  SKT3215-S-FTNGK              PIC X(09).
      *    �������e�[�u��
           03  SKT3215-KITEI-TBL       OCCURS  13.
      *    ����
               05  SKT3215-KIKENSU              PIC X(05).
      *    �_��
               05  SKT3215-KITENSU              PIC X(09).
      *    ���ҕ��S�z
               05  SKT3215-KIPTFTN              PIC X(09).
      *    �H������
               05  SKT3215-S-KIKENSU            PIC X(05).
      *    ���z
               05  SKT3215-S-KIKINGK            PIC X(09).
      *    ���S�z
               05  SKT3215-S-KIFTNGK            PIC X(09).
      *    ����e�[�u��
           03  SKT3215-KOUHI-TBL       OCCURS  2.
      *    �@�ʔԍ�
               05  SKT3215-HBTNUM               PIC X(02).
      *    ���
               05  SKT3215-KOHNAME              PIC X(10).
      *    ����
               05  SKT3215-KKENSU               PIC X(05).
      *    �_��
               05  SKT3215-KTENSU               PIC X(09).
      *    ���ҕ��S�z
               05  SKT3215-KPTFTN               PIC X(09).
      *    �H������
               05  SKT3215-S-KKENSU             PIC X(05).
      *    ���z
               05  SKT3215-S-KKINGK             PIC X(09).
      *    ���S�z
               05  SKT3215-S-KFTNGK             PIC X(09).
