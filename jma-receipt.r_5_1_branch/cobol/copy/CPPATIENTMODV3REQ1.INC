      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      ******************************************************************
      *    ���Уɴ�����Ͽ����ʴ��ܾ����(���β���V3)
      *                                   COPY (CPPATIENTMODV3REQ1.INC)
      ******************************************************************
        01  XML-PATIENTMODREQ.
            02  PTMODREQ-CONTEXT           PIC S9(9)   BINARY.
            02  PTMODREQ-OBJECT            PIC X(36).
            02  PTMODREQ-MODE              PIC S9(9)   BINARY.
            02  PTMODREQ-RECORDNAME        PIC X(128).
            02  PTMODREQ-MEDICALREQ.
      *       �ꥯ�������ֹ�
              03  PTMODREQ-REQUEST-NUMBER   PIC X(2).
      *       UID���Żҥ���ơ�
              03  PTMODREQ-KARTE-UID        PIC X(36).
      *       �����ֹ�
              03  PTMODREQ-PATIENTID        PIC X(20).
      *       ������ʬ
              03  PTMODREQ-PATIENT-MODE     PIC X(06).
      *       UID�ʥ��륫��
              03  PTMODREQ-ORCA-UID         PIC X(36).
      *       ��ǧ��å���������
              03  PTMODREQ-SELECT-ANSWER    PIC X(02).
      *       ��³��ʬ
              03  PTMODREQ-CONT-MODE        PIC X(05).
      *       ���Ծ���
              03  PTMODREQ-PTINF-INFO.
      *         ���Ի�̾
                05  PTMODREQ-NAME             PIC X(50).
      *         ���ԥ��ʻ�̾
                05  PTMODREQ-KANANAME         PIC X(50).
      *         ��ǯ����
                05  PTMODREQ-BIRTHDAY         PIC X(10).
      *         ����
                05  PTMODREQ-SEX              PIC X(01).
      *         ���Ӽ�̾
                05  PTMODREQ-SETAI-NAME       PIC X(50).
      *         ³��
                05  PTMODREQ-ZOKUGARA         PIC X(30).
      *         ����
                05  PTMODREQ-JOB              PIC X(20).
      *         �̾�
                05  PTMODREQ-NICKNAME         PIC  X(50).
      *         ���������ֹ�
                05  PTMODREQ-KEITAI-TEL       PIC  X(015).
      *         �ƣ����ֹ�
                05  PTMODREQ-FAX              PIC  X(015).
      *         �Żҥ᡼�륢�ɥ쥹
                05  PTMODREQ-EMAIL            PIC  X(050).
      *         �������
                05  PTMODREQ-ADDRESS-INFO.
      *           ͹���ֹ�
                  07  PTMODREQ-HOME-ZIP-CODE     PIC X(7).
      *           ����
                  07  PTMODREQ-HOME-ADDRESSES1   PIC X(100).
      *           �����ֹ�
                  07  PTMODREQ-HOME-ADDRESSES2   PIC X(100).
      *           �����ֹ�ʼ����
                  07  PTMODREQ-HOME-HPONE1       PIC X(15).
      *           �����ֹ��Ϣ�����
                  07  PTMODREQ-HOME-HPONE2       PIC X(15).
      *         ��̳�����
                05  PTMODREQ-OFFICE-INFO.
      *           ��̳��̾
                  07  PTMODREQ-OFFICE-NAME        PIC X(50).
      *           ͹���ֹ�
                  07  PTMODREQ-OFFICE-ZIP-CODE    PIC X(7).
      *           ����
                  07  PTMODREQ-OFFICE-ADDRESSES1  PIC X(100).
      *           �����ֹ�
                  07  PTMODREQ-OFFICE-ADDRESSES2  PIC X(100).
      *           �����ֹ�
                  07  PTMODREQ-OFFICE-HPONE       PIC X(15).
      *         Ϣ�������
                05  PTMODREQ-RENRAKU-INFO.
      *           Ϣ����̾
                  07  PTMODREQ-RENRAKU-NAME        PIC X(50).
      *           Ϣ����³��
                  07  PTMODREQ-RENRAKU-ZOKUGARA    PIC X(30).
      *           ͹���ֹ�
                  07  PTMODREQ-RENRAKU-ZIP-CODE    PIC X(7).
      *           ����
                  07  PTMODREQ-RENRAKU-ADDRESSES1  PIC X(100).
      *           �����ֹ�
                  07  PTMODREQ-RENRAKU-ADDRESSES2  PIC X(100).
      *           �����ֹ桡��
                  07  PTMODREQ-RENRAKU-HPONE1      PIC X(15).
      *           �����ֹ桡��
                  07  PTMODREQ-RENRAKU-HPONE2      PIC X(15).
      *         ���������
                05  PTMODREQ-KISEI-INFO.
      *           ������̾
                  07  PTMODREQ-KISEI-NAME          PIC X(50).
      *           ͹���ֹ�
                  07  PTMODREQ-KISEI-ZIP-CODE      PIC X(7).
      *           ����
                  07  PTMODREQ-KISEI-ADDRESSES1    PIC X(100).
      *           �����ֹ�
                  07  PTMODREQ-KISEI-ADDRESSES2    PIC X(100).
      *           �����ֹ�
                  07  PTMODREQ-KISEI-HPONE         PIC X(15).
      *         �ش���
                05  PTMODREQ-TABOO-1              PIC  X(100).
      *         �ش���
                05  PTMODREQ-TABOO-2              PIC  X(100).
      *         ����륮����
                05  PTMODREQ-ALLERGY-1            PIC  X(100).
      *         ����륮����
                05  PTMODREQ-ALLERGY-2            PIC  X(100).
      *         �����ɣ�
                05  PTMODREQ-KANSENSYO-1          PIC  X(100).
      *         �����ɣ�
                05  PTMODREQ-KANSENSYO-2          PIC  X(100).
      *         �����ȣ�
                05  PTMODREQ-COMMENT-1            PIC  X(100).
      *         �����ȣ�
                05  PTMODREQ-COMMENT-2            PIC  X(100).
      *         �ƥ��ȴ��Զ�ʬ
                05  PTMODREQ-TEST-PATIENT         PIC  X(01).
      *         ��˴��ʬ
                05  PTMODREQ-DEATHKBN             PIC  X(01).
      *         ���Ȼ�ͳ
                05  PTMODREQ-DISCOUNTKBN          PIC  X(02).
      *         ���Ψ
                05  PTMODREQ-DISCOUNT             PIC  X(02).
      *         ����
                05  PTMODREQ-CONDITION-KBN-G.
                    07  PTMODREQ-CONDITION-KBN    PIC  X(02)
                                                  OCCURS   3.
      *
             03  PTMODREQ-MODE-AREA.
      *          ����������Ͽ��ʬ
                 05  PTMODREQ-KYUSEI-MODE          PIC  X(05).
      *
      *
