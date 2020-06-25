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
      ************************************************************
      *    ��ô��ۻ����ѥѥ�᥿�������ѡ�COPY (CPORCS02.INC)
      *                                ���ѥץ������ (ORCS02)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-����    04/10/01  ���������������������б�
      * 01.00.02     NACL-����    06/03/20  ��Ω�ٱ�Ʒ׻���ʬ�ɲ�
      * 04.01.01     NACL-����    07/12/19  ������̤�������б�
      * 04.01.02     NACL-����    08/01/21  Ʊ�������������б�
      * 04.04.01     NACL-����    09/11/05  �����¾�ݸ������б�
      *
      * 04.05.01     NACL-����    09/12/04  ���������̿��ˤ�¾�ݸ��б�
      * 04.05.02     NACL-����    11/11/09  Ʊ�������������б�
      *
      * 04.06.01     NACL-����    13/02/18  �����յߵް��Ŵ����û�����
      *                                     �ɲ�
      *
      * 04.07.01     NACL-����    15/02/02  �����ƥ��������������ǽ��
      *                                     ���б�
      * 04.07.02     NACL-����    15/06/02  û���ں߼�����������������
      *                                     ������ô��׻��б�
      * 04.08.01     NACL-����    17/10/18  �������Ȱۤʤ��ݸ��Ȥ߹�碌
      *                                     �Ǥο��������б�
      * 04.08.02     NACL-����    17/12/15  �軰�Թ԰ٸ�ʪ���դ˴ؤ��
      *                                     ��ô��׻��б�
      * 04.08.03     NACL-����    18/05/15  ��ȥ饤�ե饰�ɲ�
      *                                     �����å��ѣɣģ��ɲ�
      * 04.08.04     NACL-����    18/05/31  �׻���۷�������å��ե饰�ɲ�
      *
      * 05.00.01     NACL-����    18/11/16  ���������ե饰�������������ѡ�
      *                                     �ɲ�
      ************************************************************
      *01  LNK-ORCS02-REC.
      *���ŵ��أɣ�
           03  LNK-S02-HOSPNUM             PIC  9(02).
      *������ʬ
           03  LNK-S02-NYUGAIKBN           PIC  X(01).
      *���ԣɣ�
           03  LNK-S02-PTID                PIC  9(10).
      *������ʬ�ʣ����ౡ��������������ᡢ�����쥻�ץȡ�
           03  LNK-S02-SYORI-KBN           PIC  X(01).
      *��������ʬ
           03  LNK-S02-TEIKI-KBN           PIC  X(01).
      *���ᳫ����
           03  LNK-S02-SKYSTYMD            PIC  X(08).
      *���Ὢλ��
           03  LNK-S02-SKYEDYMD            PIC  X(08).
      *���ᳫ����Ʊ���������ե饰�ʡ���
           03  LNK-S02-DOJITUFLG-ST        PIC  X(01).
      *����������������ʬ�ʣ������ʤ�������ī���顢�����뤫�顢����ͼ����ˡʡ���
           03  LNK-S02-TIMEKBN-ST          PIC  X(01).
      *���Ὢλ��Ʊ���������ե饰�ʡ���
           03  LNK-S02-DOJITUFLG-ED        PIC  X(01).
      *��λ������������ʬ�ʣ������ʤ�������ī�ޤǡ�������ޤǡ�����ͼ�ޤǡˡʡ���
           03  LNK-S02-TIMEKBN-ED          PIC  X(01).
      *����Ի��괵�Զ�ʬ
           03  LNK-S02-KOUREI-NYUIN-KBN    PIC  X(01).
      *�ü�׻��Ʒ׻���ʬ
           03  LNK-S02-TOKUSYU-SAI-KBN     PIC  X(01).
      *�����̻������ʡ���
           03  LNK-S02-NYUIN-TSUSAN-NISSU  PIC  9(04).
      *�����ƥ�����
           03  LNK-S02-SYSYMD              PIC  X(08).
      *��ȥ饤�ե饰
           03  LNK-S02-RETRY-FLG           PIC  X(01).
      *�׻���۷�������å��ե饰
           03  LNK-S02-SKYCHKFLG           PIC  X(01).
      *
      *���������ե饰�����������ѡ�
           03  LNK-S02-SYOSINFLG           PIC  X(01).
           03  LNK-S02-SYOSINFLG-DAY-TBL.
              05  LNK-S02-SYOSINFLG-DAY    PIC  X(01)   OCCURS  31.
      *���������ե饰�������������ѡ�
           03  LNK-S02-SYOSINFLG2          PIC  X(01).
      *
      *�����ݸ������ݸ��ԡˡ��̶кҳ��ܽ�ǻ�
           03  LNK-S02-SENINFLG            PIC  X(01).
      *�����ռ굻�����
           03  LNK-S02-SYUTEN              PIC  9(07).
      *�����տ��ǽ������
           03  LNK-S02-SINDAN              PIC  9(07).
      *���������ٽ������
           03  LNK-S02-MEISAI              PIC  9(07).
      *����������ô����������
           03  LNK-S02-RYOYO               PIC  9(07).
      *�����դ���¾�����ɡʣ����������������������ˤο��Ź԰٥�������
           03  LNK-S02-SONOTA.
              05  LNK-S02-SONOTA-OCC         OCCURS  50.
                07  LNK-S02-SONOTA-SRYCD     PIC  X(09).
                07  LNK-S02-SONOTA-ZAIKAISU  PIC  9(07).
                07  LNK-S02-SONOTA-TANKA     PIC  9(07).
      *�����յߵް��Ŵ����û�����
           03  LNK-S02-KYUKYU              PIC  9(07).
      *
      *������̤�����ʬ
           03  LNK-S02-NYUINDAY-TBL.
              05  LNK-S02-NYUINDAY-KBN     PIC  X(01)   OCCURS  31.
      *
      *û���ں߼���������������ʬ
           03  LNK-S02-NYUINDAY-TANKI-TBL.
              05  LNK-S02-NYUINDAY-TANKI-KBN
                                           PIC  X(01)   OCCURS  31.
      *
      *���������꼱�̶�ʬ
           03  LNK-S02-NYUSANTEI-TBL.
              05  LNK-S02-NYUSANTEI-KBN    PIC  X(01)   OCCURS  31.
      *
      *Ʊ����������ʬ
           03  LNK-S02-DOJITU.
              05  LNK-S02-DOJITU-DAY       PIC  9(01)   OCCURS  31.
              05  LNK-S02-DOJITU-DAY2      PIC  9(01)   OCCURS  31.
              05  LNK-S02-DOJITU-SKJST     PIC  X(01)   OCCURS  31.
              05  LNK-S02-DOJITU-SKJED     PIC  X(01)   OCCURS  31.
      *
      *�������Ȱۤʤ뿩�������ݸ��Ȥ߹�碌
           03  LNK-S02-SKJHKN.
              05  LNK-S02-SKJHKN-HKNCOMBI  PIC  X(04)   OCCURS  31.
              05  LNK-S02-SKJHKN-HKNCOMBI-DOU
                                           PIC  X(04)   OCCURS  31.
      *       ����ʬ���ݸ��Ȥ߹�碌
              05  LNK-S02-SKJHKN-HKNCOMBI-XXX
                                           PIC  X(04)   OCCURS  31.
      *       Ʊ��������ʬ���ݸ��Ȥ߹�碌
              05  LNK-S02-SKJHKN-HKNCOMBI-DOU-XXX
                                           PIC  X(04)   OCCURS  31.
      *
      *�������Ȱۤʤ��ݸ��Ȥ߹�碌�Ǥο�����������å��ơ��֥�
           03  LNK-S02-SKJ-SANTEI.
              05  LNK-S02-SKJ-SANTEI-OCC       OCCURS   2.
                07  LNK-S02-SKJHKN-SRYYM        PIC  X(06).
                07  LNK-S02-SKJHKN-SET          PIC  X(01)  OCCURS  31.
                07  LNK-S02-SKJHKN-AAA-TAIFLG   PIC  X(01)  OCCURS  31.
                07  LNK-S02-SKJHKN-AAA-ARIFLG   PIC  X(01)  OCCURS  31.
                07  LNK-S02-SKJHKN-BBB-TAIFLG   PIC  X(01)  OCCURS  31.
                07  LNK-S02-SKJHKN-BBB-ARIFLG   PIC  X(01)  OCCURS  31.
      *
      *�����¾�ݸ������ե饰
           03  LNK-S02-KOREI-TANYUIN       PIC  X(01).
      *
      *�軰�Թ԰�ʬ�η׻�ͥ���̤���̤Ȥʤ���ŷ�ե饰
           03  LNK-S02-DAISANYUSENFLG      PIC  X(01).
      *
      *��������
           03  LNK-S02-DAYTEN.
              05  LNK-S02-DAYTEN-OCC            OCCURS  10.
                07  LNK-S02-DAYTEN-SRYYM        PIC  X(06).
                07  LNK-S02-DAYTEN-HKNCOMBINUM  PIC  X(04).
                07  LNK-S02-DAYTEN-OCC2.
                  09  LNK-S02-DAYTEN-HKNTEN     PIC  9(07)  OCCURS  31.
      *
      *�����ޤǤΥޥ�Ĺ�ΰ�����ô��λ�
           03  LNK-S02-CYOKI.
              05  LNK-S02-CYOKI-OCC             OCCURS  10.
                07  LNK-S02-CYOKI-FLG           PIC  9(01).
                07  LNK-S02-CYOKI-ZAN1          PIC  9(07).
                07  LNK-S02-CYOKI-ZAN2          PIC  9(07).
                07  LNK-S02-CYOKI-ZAN3          PIC  9(07).
      *
      *�׻��оݤμ�Ǽ�ǡ����ɣģ�
           03  LNK-S02-IDX                      PIC  9(02).
      *
      *�����å��ѣɣģ�
           03  LNK-S02-IDX2                     PIC  9(02).
      *
      *����������
           03  LNK-S02-RC                       PIC S9(09)   BINARY.
      *