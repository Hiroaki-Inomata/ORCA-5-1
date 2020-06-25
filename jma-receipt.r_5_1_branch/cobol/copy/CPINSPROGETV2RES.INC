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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ******************************************************************
      *    ���Уɥ쥹�ݥ��ݸ��Ծ���         COPY (CPINSPROV2RES.INC)
      ******************************************************************
        01  XML-INSPROGETRES.
            02  INSPRORES-CONTEXT            PIC S9(9)   BINARY.
            02  INSPRORES-OBJECT             PIC X(36).
            02  INSPRORES-MODE               PIC S9(9)   BINARY.
            02  INSPRORES-RECORDNAME         PIC X(128).
            02  INSPRORES-APPOINTRES.
      *       �»���
              03  INSPRORES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              03  INSPRORES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              03  INSPRORES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              03  INSPRORES-API-RESULT-MSG      PIC X(100).
      *
              03  INSPRORES-RESKEY              PIC X(100).
      *       �ݸ����ֹ����
              03  INSPRORES-INSURANCE-INFOMATION.
                  05  INSPRORES-INSURANCE-OCC   OCCURS   2500.
      *               �ݸ����ֹ�
                      07  INSPRORES-INSPRO-NUMBER    PIC X(08).
      *               �ݸ���̾
                      07  INSPRORES-INSPRO-NAME      PIC X(200).
      *               �ݸ���̾ά�Σ�
                      07  INSPRORES-INSPRO-NAME1     PIC X(100).
      *               �ݸ���̾ά�Σ�
                      07  INSPRORES-INSPRO-NAME2     PIC X(40).
      *               �ݸ���̾ά�Σ�
                      07  INSPRORES-INSPRO-NAME3     PIC X(40).
      *               �ݸ��ֹ�
                      07  INSPRORES-INS-NUMBER       PIC X(03).
      *               �ݸ��ֹ�̾��
                      07  INSPRORES-INS-NUMBER-NAME  PIC X(20).
      *               ͹���ֹ�
                      07  INSPRORES-INSE-ZIP-CODE      PIC X(7).
      *               ����
                      07  INSPRORES-INS-ADDRESSES1    PIC X(200).
      *               �����ֹ�
                      07  INSPRORES-INS-ADDRESSES2    PIC X(200).
      *               �����ֹ�
                      07  INSPRORES-INS-HPONE         PIC X(15).
      *               ����
                      07  INSPRORES-INS-SYMBOL        PIC X(80).
      *               ���ճ����ܿ͡������
                      07  INSPRORES-HON-GAIKYURATE    PIC X(03).
      *               ���ճ����ܿ͡�������
                      07  INSPRORES-HON-NYUKYURATE    PIC X(03).
      *               ���ճ��ʲ�²�������
                      07  INSPRORES-KZK-GAIKYURATE    PIC X(03).
      *               ���ճ��ʲ�²��������
                      07  INSPRORES-KZK-NYUKYURATE    PIC X(03).
      *               ��ư��ʬ
                      07  INSPRORES-CHANGE-MEMO       PIC X(10).
      *               ��ưǯ����
                      07  INSPRORES-CHANGE-DATE       PIC X(10).
      *
      *