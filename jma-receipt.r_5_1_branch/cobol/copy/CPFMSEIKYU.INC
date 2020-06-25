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
        01  FMSEIKYU-REC.
            03  FMSEIKYU-CONTEXT                   PIC S9(9)   BINARY.
            03  FMSEIKYU-OBJECT                    PIC X(36).
            03  FMSEIKYU-MODE                      PIC S9(9)   BINARY.
            03  FMSEIKYU-RECORDNAME                PIC X(128).
            03  FMSEIKYU-DATA.
              05  FMSEIKYU-FORM-ID               PIC X(30).
              05  FMSEIKYU-CUSTOM-ID               PIC X(30).
              05  FMSEIKYU-PRINTER                 PIC X(20).
      *
              05  FMSEIKYU-MOTOID                  PIC X(02).
      *
      *       �����ֹ�
              05  FMSEIKYU-PTNUM                   PIC X(20).
      *       ���Ի�̾
              05  FMSEIKYU-NAME                    PIC X(50).
      *       ���ԥ��ʻ�̾
              05  FMSEIKYU-KANANAME                PIC X(50).
      *
      *       ���ŵ��ؾ���
              05  FMSEIKYU-HOSP-AREA.
      *         ���ŵ���̾��
                07  FMSEIKYU-HOSP-NAME-G.
                  09  FMSEIKYU-HOSP-NAME           PIC X(120)
                                                   OCCURS   3.
      *         ���ŵ���͹���ֹ�
                07  FMSEIKYU-HOSP-POST             PIC X(7).
      *         ���ŵ��ؽ���
                07  FMSEIKYU-HOSP-ADRS-G.
                  09  FMSEIKYU-HOSP-ADRS           PIC X(200)
                                                   OCCURS   3.
      *         ���ŵ��������ֹ�
                07  FMSEIKYU-HOSP-TEL              PIC X(15).
      *
      *       ������
              05  FMSEIKYU-JNISSU                  PIC X(03).
      *       ���Ŵ��ֶ�ʬ
              05  FMSEIKYU-SRYKIKANKBN             PIC X(01).
      *       ����ǯ��
              05  FMSEIKYU-SRYYM                   PIC X(07).
      *       ���Ͽ�����
              05  FMSEIKYU-STSRYYMD                PIC X(10).
      *       ��λ������
              05  FMSEIKYU-EDSRYYMD                PIC X(10).
      *       ��ɼȯ����
              05  FMSEIKYU-DENPPRTYMD              PIC X(10).
      *
      *       ���Ų�
              05  FMSEIKYU-SRYKANAME                   PIC X(30).
      *       �ݸ�����
              05  FMSEIKYU-HOKENNAME                   PIC X(40).
              05  FMSEIKYU-FUTAN-G.
      *         ��ô���
                07  FMSEIKYU-FUTAN                     PIC X(04).
      *         ��ô���
                07  FMSEIKYU-FUTANMEI                  PIC X(02).
      *       ��ɼ�ֹ�ʰ��ʳ���
              05  FMSEIKYU-DENPNUM                     PIC X(07).
      *       ����ɽ����ʬ
              05  FMSEIKYU-TEISEI-KBN                  PIC X(01).
      *       �������
              05  FMSEIKYU-MEISAI-OCC                  OCCURS 16.
      *           ������ܶ�ʬ
                  07  FMSEIKYU-HKNTEN-KBN              PIC X(03).
      *           �������̾
                  07  FMSEIKYU-HKNTEN-NAME             PIC X(20).
      *           ��������
                  07  FMSEIKYU-HKNTEN                  PIC X(08).
      *           �����ۡ��ݸ�Ŭ�ѳ���
                  07  FMSEIKYU-TGMONEY                 PIC X(08).
      *       �������
              05  FMSEIKYU-TOTAL-TEN                   PIC X(08).
      *       ���԰�����ô��
              05  FMSEIKYU-HKNTEKMONEY                 PIC X(08).
      *       ���԰�����ô���٥��ϫ�ҿ��������Ʒǡ�
              05  FMSEIKYU-LBL-RSI-TOTAL               PIC X(20).
      *       ���԰�����ô���ϫ�ҿ��������Ʒǡ�
              05  FMSEIKYU-RSI-TOTAL                   PIC X(08).
      *       ���������ô��
              05  FMSEIKYU-KOH-FTN                     PIC X(08).
      *       �ݸ�Ŭ�ѳ����
              05  FMSEIKYU-TOTAL-TGMONEY               PIC X(08).
      *       �ݸ�Ŭ�ѳ���׾����ǺƷ�
              05  FMSEIKYU-TOTAL-TGMONEY-TAX           PIC X(08).
      *       ����¾����
              05  FMSEIKYU-JIHI-OCC                    OCCURS  10.
      *             ����¾����̾��
                07  FMSEIKYU-JIHI-NAME                 PIC X(20).
      *             ����¾������
                07  FMSEIKYU-JIHI                      PIC X(08).
      *       ����¾�����׶��
              05  FMSEIKYU-JIHI-TOTAL                  PIC X(08).
      *       ����¾�����׶�۾����ǺƷ�
              05  FMSEIKYU-JIHI-TOTAL-TAX              PIC X(08).
      *       Ĵ���⣱
              05  FMSEIKYU-CHOSEI1                     PIC X(08).
      *       Ĵ���⣲
              05  FMSEIKYU-CHOSEI2                     PIC X(08).
      *       ���������
              05  FMSEIKYU-SKYMONEY                    PIC X(08).
      *       ��������۾����ǺƷ�
              05  FMSEIKYU-SKYMONEY-TAX                PIC X(08).
      *       ���������
              05  FMSEIKYU-SKYMONEY-LAST               PIC X(08).
      *       ��������
              05  FMSEIKYU-SKYMONEY-TOTAL              PIC X(08).
      *       ���������
              05  FMSEIKYU-NYUKIN                      PIC X(08).
      *
      *       ���ս񤭡��������ʬ��
              05  FMSEIKYU-SKY-CAUTION                 PIC X(100).
      *
      *       ���
              05  FMSEIKYU-MEMO                        PIC X(80)
                                                       OCCURS 10.
      *
      *       ���پ���
              05   FMSEIKYU-SRY-ITEM-AREA              OCCURS 2000.
      *         ���롼�ץإå��ʣ�����ɼ�ֹ椬�Ѥ���٤���Ƭ��
                07   FMSEIKYU-GRPHEAD                  PIC X(01).
      *         Ϣ�֡���ɼ�ֹ椬�Ѥ���٤����áˡ�999����ɼ����ԡ�
                07   FMSEIKYU-GRPNUM                   PIC X(03).
      *         ���Ŷ�ʬ̾
                07   FMSEIKYU-SRYKBN                   PIC X(10).
      *         ̾��
                07  FMSEIKYU-SRYNAME-G.
      *           �޳��ϡʡ��ʤ�
                  09  FMSEIKYU-ZAIHEN                  PIC X(02).
      *           ��������̾��
                  09  FMSEIKYU-SRYNAME                 PIC X(148).
      *         �������ܱ�Ω�ƥե饰
                07   FMSEIKYU-TENSU-MARK               PIC X(1).
      *         ����
                07   FMSEIKYU-TENSU                    PIC X(09).
      *         ���
                07   FMSEIKYU-KAISU                    PIC X(03).
      *         ���
                07   FMSEIKYU-TOTAL                    PIC X(11).
      *         �޽�λ�ʣ����޺ǽ��ԡ�
                07  FMSEIKYU-ZAIEND                    PIC X(01).
      *
      *       ���پ������Խ��ե饰
              05  FMSEIKYU-YEN-FLG                     PIC X(1).
              05  FMSEIKYU-SRY-CAUTION                 PIC X(100).