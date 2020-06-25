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
        01  FMSEIKYUN-REC.
            03  FMSEIKYUN-CONTEXT              PIC S9(9)   BINARY.
            03  FMSEIKYUN-OBJECT               PIC X(36).
            03  FMSEIKYUN-MODE                 PIC S9(9)   BINARY.
            03  FMSEIKYUN-RECORDNAME           PIC X(128).
            03  FMSEIKYUN-DATA.
              05  FMSEIKYUN-FORM-ID            PIC X(30).
              05  FMSEIKYUN-CUSTOM-ID          PIC X(30).
              05  FMSEIKYUN-PRINTER            PIC X(20).
      *
              05  FMSEIKYUN-MOTOID             PIC X(02).
              05  FMSEIKYUN-KARIKEISANKBN      PIC X(01).
      *
      *       �����ֹ�
              05  FMSEIKYUN-PTNUM              PIC X(20).
      *       ���Ի�̾
              05  FMSEIKYUN-NAME               PIC X(50).
      *       ���ԥ��ʻ�̾
              05  FMSEIKYUN-KANANAME           PIC X(50).
      *
      *       ���ŵ��ؾ���
              05  FMSEIKYUN-HOSP-AREA.
      *         ���ŵ���̾��
                07  FMSEIKYUN-HOSP-NAME-G.
                  09  FMSEIKYUN-HOSP-NAME      PIC X(120)
                                               OCCURS   3.
      *         ���ŵ���͹���ֹ�
                07  FMSEIKYUN-HOSP-POST        PIC X(7).
      *         ���ŵ��ؽ���
                07  FMSEIKYUN-HOSP-ADRS-G.
                  09  FMSEIKYUN-HOSP-ADRS      PIC X(200)
                                               OCCURS   3.
      *         ���ŵ��������ֹ�
                07  FMSEIKYUN-HOSP-TEL         PIC X(15).
      *       ���Ͽ�����
              05  FMSEIKYUN-STSRYYMD           PIC X(10).
      *       ��λ������
              05  FMSEIKYUN-EDSRYYMD           PIC X(10).
      *       ��ɼȯ����
              05  FMSEIKYUN-DENPPRTYMD         PIC X(10).
      *       �����ֹ�
              05  FMSEIKYUN-BTUNUM             PIC X(2).
      *       ����̾��
              05  FMSEIKYUN-BTUNAME            PIC X(20).
      *       �¼��ֹ�
              05  FMSEIKYUN-BRMNUM             PIC X(6).
      *
      *       ���Ų�
              05  FMSEIKYUN-SRYKANAME          PIC X(30).
      *       �ݸ�����
              05  FMSEIKYUN-HOKENNAME          PIC X(40).
              05  FMSEIKYUN-FUTAN-G.
      *         ��ô���
                07  FMSEIKYUN-FUTAN            PIC X(04).
      *         ��ô���
                07  FMSEIKYUN-FUTANMEI         PIC X(02).
      *       ��ɼ�ֹ�ʰ��ʳ���
              05  FMSEIKYUN-DENPNUM            PIC X(07).
      *       ����ɽ����ʬ
              05  FMSEIKYUN-TEISEI-KBN         PIC X(01).
      *       �������
              05  FMSEIKYUN-MEISAI-OCC         OCCURS 16.
      *           ������ܶ�ʬ
                  07  FMSEIKYUN-HKNTEN-KBN     PIC X(03).
      *           �������̾
                  07  FMSEIKYUN-HKNTEN-NAME    PIC X(20).
      *           ��������
                  07  FMSEIKYUN-HKNTEN         PIC X(10).
      *           �����ۡ��ݸ�Ŭ�ѳ���
                  07  FMSEIKYUN-TGMONEY        PIC X(10).
      *       �������
              05  FMSEIKYUN-TOTAL-TEN          PIC X(10).
      *       ���԰�����ô��
              05  FMSEIKYUN-HKNTEKMONEY                 PIC X(10).
      *       ���԰�����ô���٥��ϫ�ҿ��������Ʒǡ�
              05  FMSEIKYUN-LBL-RSI-TOTAL               PIC X(20).
      *       ���԰�����ô���ϫ�ҿ��������Ʒǡ�
              05  FMSEIKYUN-RSI-TOTAL                   PIC X(10).
      *       ���������ô��
              05  FMSEIKYUN-KOH-FTN                     PIC X(10).
      *       ����������
              05  FMSEIKYUN-SKJRYOYO                    PIC X(10).
      *       ������ô��
              05  FMSEIKYUN-SKJ-FTN                     PIC X(10).
      *       ����������
              05  FMSEIKYUN-LIFERYOYO                    PIC X(10).
      *       ������ô��
              05  FMSEIKYUN-LIFE-FTN                     PIC X(10).
      *       �ݸ�Ŭ�ѳ����
              05  FMSEIKYUN-TOTAL-TGMONEY               PIC X(10).
      *       �ݸ�Ŭ�ѳ���׾����ǺƷ�
              05  FMSEIKYUN-TOTAL-TGMONEY-TAX           PIC X(10).
      *       ����¾����
              05  FMSEIKYUN-JIHI-OCC                    OCCURS  10.
      *             ����¾����̾��
                07  FMSEIKYUN-JIHI-NAME                 PIC X(20).
      *             ����¾������
                07  FMSEIKYUN-JIHI                      PIC X(10).
      *       ����¾�����׶��
              05  FMSEIKYUN-JIHI-TOTAL                  PIC X(10).
      *       ����¾�����׶�۾����ǺƷ�
              05  FMSEIKYUN-JIHI-TOTAL-TAX              PIC X(10).
      *       ��������
              05  FMSEIKYUN-RMSAGAKU                    PIC X(10).
      *       �������۾����ǺƷ�
              05  FMSEIKYUN-RMSAGAKU-TAX                PIC X(10).
      *       Ĵ���⣱
              05  FMSEIKYUN-CHOSEI1                     PIC X(10).
      *       Ĵ���⣲
              05  FMSEIKYUN-CHOSEI2                     PIC X(10).
      *       ���������
              05  FMSEIKYUN-SKYMONEY                    PIC X(10).
      *       ��������۾����ǺƷ�
              05  FMSEIKYUN-SKYMONEY-TAX                PIC X(10).
      *       ���������
              05  FMSEIKYUN-NYUKIN                      PIC X(10).
      *       ����̤����
              05  FMSEIKYUN-NYU-MISYU                   PIC X(10).
      *       ����̤����
              05  FMSEIKYUN-GAI-MISYU                   PIC X(10).
      *
      *       ���ս񤭡��������ʬ��
              05  FMSEIKYUN-SKY-CAUTION                 PIC X(100).
      *
      *       ���
              05  FMSEIKYUN-MEMO                        PIC X(80)
                                                        OCCURS 10.