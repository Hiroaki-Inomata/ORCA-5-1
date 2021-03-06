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
      *    ���Уɴ�����Ͽ����ʴ��Զش����ޡ�(���β���V3)
      *                                   COPY (CPPATIENTMODV3RES7.INC)
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
      *      �����ֹ�
             03  PTMODRES-PATIENTID        PIC X(20).
      *      ���Ծ���
             03  PTMODRES-PTINF-INFO.
      *        ���Ի�̾
               05  PTMODRES-NAME             PIC X(100).
      *        ���ԥ��ʻ�̾
               05  PTMODRES-KANANAME         PIC X(100).
      *        ��ǯ����
               05  PTMODRES-BIRTHDAY         PIC X(10).
      *        ����
               05  PTMODRES-SEX              PIC X(01).
      *
      *       ���Զش����޾���
              03  PTMODRES-PTKINKI-INFO-G.
      *          ���Զش����޾���
                 05  PTMODRES-PTKINKI-G       OCCURS   500.
      *              �ش����ޥ�����
                     07  PTMODRES-DRUGCD-CODE      PIC X(09).
      *              �ش�����̾��
                     07  PTMODRES-DRUGCD-NAME      PIC X(80).
      *              ͭ����λ���ʥޥ�����λ����
                     07  PTMODRES-DRUGCD-END-DATE  PIC X(10).
      *              �в��������ܹܰԥ�����
                     07  PTMODRES-CHANGE-CODE      PIC X(09).
                     07  PTMODRES-CHANGE-NAME      PIC X(80).
      *              �ش�������
                     07  PTMODRES-START-DATE       PIC X(10).
      *              ��Ϳ��
                     07  PTMODRES-TOUYO-DATE       PIC X(10).
      *              ���顼������
                     07  PTMODRES-ERRCD            PIC X(04).
                     07  PTMODRES-ERRCD-MSG        PIC X(100).
      *
