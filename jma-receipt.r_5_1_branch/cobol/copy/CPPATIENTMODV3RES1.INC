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
      *                                   COPY (CPPATIENTMODV3RES1.INC)
      ******************************************************************
       01  XML-PATIENTMODRES.
           02  PTMODRES-CONTEXT           PIC S9(9)   BINARY.
           02  PTMODRES-OBJECT            PIC X(36).
           02  PTMODRES-MODE              PIC S9(9)   BINARY.
           02  PTMODRES-RECORDNAME        PIC X(128).
           02  PTMODRES-APPOINTRES.
      *      �ꥯ�������ֹ�
             03  PTMODRES-REQUEST-NUMBER    PIC X(2).
      *      �쥹�ݥ��ֹ�
             03  PTMODRES-RESPONSE-NUMBER   PIC X(2).
      *      UID���Żҥ���ơ�
             03  PTMODRES-KARTE-UID         PIC X(36).
      *      UID�ʥ��륫��
             03  PTMODRES-ORCA-UID          PIC X(36).
      *      ��³��ʬ
             03  PTMODRES-CONT-MODE         PIC X(05).
      *      �»���
             03  PTMODRES-INFORMATION-DATE  PIC X(10).
      *      �»ܻ���
             03  PTMODRES-INFORMATION-TIME  PIC X(8).
      *      �ֵѥ�����
             03  PTMODRES-API-RESULT        PIC X(3).
      *      �ֵѥ�å�����
             03  PTMODRES-API-RESULT-MSG    PIC X(100).
      *
             03  PTMODRES-RESKEY              PIC X(100).
      *      ���Ծ���
             03  PTMODRES-PTINF-INFO.
      *        �����ֹ�
               05  PTMODRES-PATIENTID        PIC X(20).
      *        ���Ի�̾
               05  PTMODRES-NAME             PIC X(50).
      *        ���ԥ��ʻ�̾
               05  PTMODRES-KANANAME         PIC X(50).
      *        ��ǯ����
               05  PTMODRES-BIRTHDAY         PIC X(10).
      *        ����
               05  PTMODRES-SEX              PIC X(01).
      *        ���Ӽ�̾
               05  PTMODRES-SETAI-NAME       PIC X(50).
      *        ³��
               05  PTMODRES-ZOKUGARA         PIC X(30).
      *        ����
               05  PTMODRES-JOB              PIC X(20).
      *        �̾�
               05  PTMODRES-NICKNAME         PIC  X(50).
      *        ���������ֹ�
               05  PTMODRES-KEITAI-TEL       PIC  X(015).
      *        �ƣ����ֹ�
               05  PTMODRES-FAX              PIC  X(015).
      *        �Żҥ᡼�륢�ɥ쥹
               05  PTMODRES-EMAIL            PIC  X(050).
      *        �������
               05  PTMODRES-ADDRESS-INFO.
      *            ͹���ֹ�
                   07  PTMODRES-HOME-ZIP-CODE     PIC X(7).
      *            ����
                   07  PTMODRES-HOME-ADDRESSES1   PIC X(100).
      *            �����ֹ�
                   07  PTMODRES-HOME-ADDRESSES2   PIC X(100).
      *            �����ֹ�ʼ����
                   07  PTMODRES-HOME-HPONE1       PIC X(15).
      *            �����ֹ��Ϣ�����
                   07  PTMODRES-HOME-HPONE2       PIC X(15).
      *        ��̳�����
               05  PTMODRES-OFFICE-INFO.
      *            ��̳��̾
                   07  PTMODRES-OFFICE-NAME        PIC X(50).
      *            ͹���ֹ�
                   07  PTMODRES-OFFICE-ZIP-CODE    PIC X(7).
      *            ����
                   07  PTMODRES-OFFICE-ADDRESSES1  PIC X(100).
      *            �����ֹ�
                   07  PTMODRES-OFFICE-ADDRESSES2  PIC X(100).
      *            �����ֹ�
                   07  PTMODRES-OFFICE-HPONE        PIC X(15).
      *         Ϣ�������
                05  PTMODRES-RENRAKU-INFO.
      *           Ϣ����̾
                  07  PTMODRES-RENRAKU-NAME        PIC X(50).
      *           Ϣ����³��
                  07  PTMODRES-RENRAKU-ZOKUGARA    PIC X(30).
      *           ͹���ֹ�
                  07  PTMODRES-RENRAKU-ZIP-CODE    PIC X(7).
      *           ����
                  07  PTMODRES-RENRAKU-ADDRESSES1  PIC X(100).
      *           �����ֹ�
                  07  PTMODRES-RENRAKU-ADDRESSES2  PIC X(100).
      *           �����ֹ桡��
                  07  PTMODRES-RENRAKU-HPONE1      PIC X(15).
      *           �����ֹ桡��
                  07  PTMODRES-RENRAKU-HPONE2      PIC X(15).
      *         ���������
                05  PTMODRES-KISEI-INFO.
      *           ��̳��̾
                  07  PTMODRES-KISEI-NAME          PIC X(50).
      *           ͹���ֹ�
                  07  PTMODRES-KISEI-ZIP-CODE      PIC X(7).
      *           ����
                  07  PTMODRES-KISEI-ADDRESSES1    PIC X(100).
      *           �����ֹ�
                  07  PTMODRES-KISEI-ADDRESSES2    PIC X(100).
      *           �����ֹ�
                  07  PTMODRES-KISEI-HPONE         PIC X(15).
      *        �ش���
               05  PTMODRES-TABOO-1              PIC  X(100).
      *        �ش���
               05  PTMODRES-TABOO-2              PIC  X(100).
      *        ����륮����
               05  PTMODRES-ALLERGY-1            PIC  X(100).
      *        ����륮����
               05  PTMODRES-ALLERGY-2            PIC  X(100).
      *        �����ɣ�
               05  PTMODRES-KANSENSYO-1          PIC  X(100).
      *        �����ɣ�
               05  PTMODRES-KANSENSYO-2          PIC  X(100).
      *        �����ȣ�
               05  PTMODRES-COMMENT-1            PIC  X(100).
      *        �����ȣ�
               05  PTMODRES-COMMENT-2            PIC  X(100).
      *        �ƥ��ȴ��Զ�ʬ
               05  PTMODRES-TEST-PATIENT         PIC  X(01).
      *        ��˴��ʬ
               05  PTMODRES-DEATHKBN             PIC  X(01).
      *        ���Ȼ�ͳ
               05  PTMODRES-DISCOUNTKBN          PIC  X(02).
               05  PTMODRES-DISCOUNTKBN-NAME     PIC  X(20).
      *        ���Ψ
               05  PTMODRES-DISCOUNT             PIC  X(02).
               05  PTMODRES-DISCOUNT-NAME        PIC  X(14).
      *        ���֣����飳
               05  PTMODRES-CONDITION-KBN-G.
                    07  PTMODREQ-CONDITION-OCC    OCCURS   3.
                        09  PTMODRES-CONDITION-KBN     PIC X(02).
                        09  PTMODRES-CONDITION-NAME    PIC X(20).
      *
      *      Ʊ�촵�Ծ���
             03  PTMODRES-JUFUKU-PATIENT-G.
                 05  PTMODRES-JUFUKU-PATIENT-INFO   OCCURS   20.
      *              �����ֹ�
                     07  PTMODRES-JFK-PATIENTID      PIC X(20).
      *              ���Ի�̾
                     07  PTMODRES-JFK-NAME           PIC X(50).
      *              ���ԥ��ʻ�̾
                     07  PTMODRES-JFK-KANANAME       PIC X(50).
      *              ��ǯ����
                     07  PTMODRES-JFK-BIRTHDAY       PIC X(10).
      *              ����
                     07  PTMODRES-JFK-SEX            PIC X(01).
      *              ����
                     07  PTMODRES-JFK-HOME-ADRS      PIC X(100).
      *              �����ֹ�
                     07  PTMODRES-JFK-HOME-BANTI     PIC X(100).
      *
      *      ���顼��å�����
             03  PTMODRES-PATIENT-ERRMSG-G.
                 05  PTMODRES-PATIENT-ERRMSG-INF  OCCURS   50.
                     07  PTMODRES-ERROR        PIC X(04).
                     07  PTMODRES-ERROR-MSG    PIC X(200).
      *      ��˥󥰥�å�����
             03  PTMODRES-WARNING-MSG-G.
                 05  PTMODRES-WARNING-INF    OCCURS   50.
                     07  PTMODRES-WARNING      PIC X(04).
                     07  PTMODRES-WARNING-MSG  PIC X(200).
      *
      *      ��ǧ��å�����
             03  PTMODRES-SELECT-G.
                 05  PTMODRES-SELECT-CODE      PIC X(04).
                 05  PTMODRES-SELECT-MSG       PIC X(200).
      *