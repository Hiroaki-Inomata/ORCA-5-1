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
      *    ��ô��ۻ����ѥѥ�᥿���������� COPY (CPORCS01.INC)
      *                                ���ѥץ������  (ORCS01)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  03/04/16  �����ա������ݸ��ɲ�
      * 01.00.02     NACL-����    04/06/30  �쥤�����Ƚ���
      * 01.00.03     NACL-����    04/10/01  ���������������������б�
      * 01.00.04     NACL-����    06/03/22  ��Ω�ٱ�Ʒ׻���ʬ�ɲ�
      *                                     �ü�׻��Ʒ׻���ʬ�ɲ�
      *                                     ��Ω�ٱ���ô�ѹ���ʬ�ɲ�
      * 01.00.05     NACL-����    09/06/16  ���Ʒ׻��硢���Ʒ׻���ˡ�ɲ�
      * 01.00.06     NACL-����    10/01/26  �ǻ�������ե饰���ɲ�
      * 01.00.07     NACL-����    11/02/03  ���ֳ����ǥե饰�ɲ�
      * 01.00.08     NACL-����    12/03/09  ��ʪ���շ׻��ե饰�ɲ�
      *                                     ��ʪ���շ׻��ޤȤ����ե饰�ɲ�
      *                                     ��ʪ���ե�å������ե饰�ɲ�
      *
      * 04.06.01     NACL-����    13/02/18  �����յߵް��Ŵ����û������ɲ�
      * 04.08.01     NACL-����    17/07/14  �����å��ѣɣģ��ɲ�
      * 04.08.02     NACL-����    17/12/15  �軰�Թ԰ٸ�ʪ���դ˴ؤ��
      *                                     ��ô��׻��б�
      * 04.08.03     NACL-����    18/05/15  ��ȥ饤�ե饰�ɲ�
      * 04.08.04     NACL-����    18/05/31  �׻���۷�������å��ե饰�ɲ�
      *
      * 05.00.01     NACL-����    18/11/16  ���������ե饰�������������ѡ�
      *                                     �ɲ�
      * 05.00.02     NACL-����    19/10/01  ���Ʒ׻��ޥ�Ĺ������ôͥ��׻�
      *                                     �ե饰�ɲ�
      ************************************************************
       01  LNK-ORCS01-REC.
      *
      *�Ʒ׻�������ʬ
           03  LNK-S01-SAIKEISAN-KBN       PIC  X(01).
      *����ԡʺ������硦���������˻��괵�Զ�ʬ
           03  LNK-S01-KOUREI-ZAITAKU-KBN  PIC  X(01).
      *�ü�׻��Ʒ׻���ʬ
           03  LNK-S01-TOKUSYU-SAI-KBN     PIC  X(01).
      *ʣ���ʤޤȤὸ�ץե饰�ʣ����ޤȤ�׻����롡�������ʤ���
           03  LNK-S01-KAGRPFLG            PIC  X(01).
      *���������ե饰�����������ѡ�
           03  LNK-S01-SYOSINFLG           PIC  X(01).
      *���������ե饰�������������ѡ�
           03  LNK-S01-SYOSINFLG2          PIC  X(01).
      *�ǻ�������ե饰�����������ѡ�
           03  LNK-S01-SINSATUFLG          PIC  X(01).
      *�ǻ�������ե饰�������������ѡ�
           03  LNK-S01-SINSATUFLG2         PIC  X(01).
      *���ֳ����ǥե饰�����������ѡ�
           03  LNK-S01-JIKANGAI            PIC  X(01).
      *�軰�Թ԰�ʬ�׻�ͥ��ե饰
           03  LNK-S01-DAISANYUSENFLG      PIC  X(01).
      *���Ʒ׻���
           03  LNK-S01-IKTSORTKBN          PIC  X(01).
      *���Ʒ׻���ˡ
           03  LNK-S01-IKTKEISANKBN        PIC  X(01).
      *ʬ�ڼ����ʬ
           03  LNK-S01-BUNBENJIHIKBN       PIC  X(01).
      *
      *���Ʒ׻�����ʪ���շ׻��ե饰
           03  LNK-S01-IKTGENKEIFLG        PIC  X(01).
      *���Ʒ׻�����ʪ���շ׻��ޤȤ����ե饰
           03  LNK-S01-IKTGENMKJFLG        PIC  X(01).
      *���Ʒ׻����»ܥե饰
           03  LNK-S01-IKTJISSIFLG         PIC  X(01).
      *���Ʒ׻��������ԥ����å��ե饰
           03  LNK-S01-IKTCHECKFLG         PIC  X(01).
      *���Ʒ׻����ޥ�Ĺ������ôͥ��׻��ե饰
           03  LNK-S01-IKTCYOKIFLG         PIC  X(01).
      *
      *��ʪ���ե�å������ե饰
           03  LNK-S01-GENBUTU-MSG-FLG     PIC  X(01).
      *��ȥ饤�ե饰
           03  LNK-S01-RETRY-FLG           PIC  X(01).
      *�׻���۷�������å��ե饰
           03  LNK-S01-SKYCHKFLG           PIC  X(01).
      *
           03  LNK-S01-SINKOUI.
      *���Ź԰����� 
             04  LNK-S01-TBL1              OCCURS   3.
      *���Ź԰����� 
               05  LNK-S01-TBL             OCCURS  40.
      *���Ŷ�ʬ
                 06  LNK-S01-SRYKBN        PIC  X(02).
      *���ֹ�
                 06  LNK-S01-ZAIBAN        PIC  9(08).
      *���ť�����
                 06  LNK-S01-SRYCD         PIC  X(09).
      *����
                 06  LNK-S01-SURYOU        PIC  9(05)V9(05).
      *���
                 06  LNK-S01-KAISUU        PIC  9(03).
      *����
                 06  LNK-S01-TENSUU        PIC  9(07).
      *
           03  LNK-S01-OCC                 OCCURS  15.
      *�����ݸ������ݸ��ԡˡ��̶кҳ��ܽ�ǻ�
             04  LNK-S01-SENINFLG          PIC  X(01).
      *�����ռ굻�����
             04  LNK-S01-SYUTEN            PIC  9(07).
      *�����տ��ǽ������
             04  LNK-S01-SINDAN            PIC  9(07).
      *���������ٽ������
             04  LNK-S01-MEISAI            PIC  9(07).
      *����������ô����������
             04  LNK-S01-RYOYO             PIC  9(07).
      *�����դ���¾�����ɡʣ����������������������ˤο��Ź԰٥�������
             04  LNK-S01-SONOTA.
               05  LNK-S01-SONOTA-OCC         OCCURS  20.
                 06  LNK-S01-SONOTA-SRYCD     PIC  X(09).
                 06  LNK-S01-SONOTA-ZAIKAISU  PIC  9(07).
                 06  LNK-S01-SONOTA-TANKA     PIC  9(07).
      *�����յߵް��Ŵ����û�����
             04  LNK-S01-KYUKYU            PIC  9(07).
      *
      *�׻��оݤμ�Ǽ�ǡ����ɣģ�
           03  LNK-S01-IDX                 PIC  9(02).
      *
      *�����å��ѣɣģ�
           03  LNK-S01-IDX2                PIC  9(02).
      *
           03  LNK-S01-OCC2                OCCURS  15.
      *��ץ쥳���ɤ��ĥ쥳���ɼ��̥ե饰
             04  LNK-S01-TTLARIFLG         PIC  X(01).
      *
      *����������
           03  LNK-S01-RC                  PIC S9(09)   BINARY.
      *