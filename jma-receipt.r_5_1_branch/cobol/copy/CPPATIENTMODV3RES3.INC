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
      *    ���Уɴ�����Ͽ�����ϫ���ݸ������(���β���V3)
      *                                   COPY (CPPATIENTMODV3RES3.INC)
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
             03  PTMODRES-RESKEY           PIC X(100).
      *      �������
             03  PTMODRES-BASE-DATE        PIC X(10).
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
      *        �������
               05  PTMODRES-ADDRESS-INFO.
      *            ����
                   07  PTMODRES-HOME-ADDRESSES1   PIC X(200).
      *            �����ֹ�
                   07  PTMODRES-HOME-ADDRESSES2   PIC X(200).
      *
      *      ϫ���ݸ�����
             03  PTMODRES-ACCIDENT-INFO-G.
                 05  PTMODRES-ACCIDENT-INFO     OCCURS   40.
      *            ������ʬ
                   07  PTMODRES-MAIN-MODE              PIC X(06).
      *            ϫ���ݸ��ɣ�
                   07  PTMODRES-MAIN-INSURANCE-ID      PIC X(10).
      *            ����ɣġ��軰�Թ԰١�
                   07  PTMODRES-INSURANCE-ID           PIC X(10).
      *            �ݸ��μ���
                   07  PTMODRES-INSURANCE-CLASS        PIC X(03).
      *            �ݸ��μ���̾��
                   07  PTMODRES-INSURANCE-NAME         PIC X(20).
      *ϫ�Ҿ���
      *            ϫ���ݸ���ʬ
                   07  PTMODRES-ACCIDENT               PIC X(01).
      *            ϫ���ݸ�̾��
                   07  PTMODRES-ACCIDENT-NAME          PIC X(50).
      *            ���¤�����
                   07  PTMODRES-DISEASE-LOCATION       PIC X(100).
      *            ����ǯ����
                   07  PTMODRES-DISEASE-DATE           PIC X(10).
      *            ���ܳ�����
                   07  PTMODRES-START-DATE             PIC X(10).
      *            ���ܽ�λ��
                   07  PTMODRES-END-DATE               PIC X(10).
      *ϫ��
      *            ϫƯ�ݸ��ֹ�
                   07  PTMODRES-ACCIDENT-NUMBER        PIC X(14).
      *            ǯ��ڽ��ֹ�
                   07  PTMODRES-PENSIONCERTIFICATE     PIC X(09).
      *            �ҳ���ʬ
                   07  PTMODRES-ACCIDENT-CLASS         PIC X(01).
      *            �ҳ���ʬ̾��
                   07  PTMODRES-ACCIDENT-CLASS-NAME    PIC X(20).
      *            ϫƯ�����Ľ𥳡���
                   07  PTMODRES-LABOR-ST-CODE          PIC X(05).
      *            ϫƯ�����Ľ𥳡���
                   07  PTMODRES-LABOR-ST-NAME          PIC X(50).
      *
      *            ���Ѻ��̶�ʬ
                   07  PTMODRES-SINKEI                 PIC X(01).
                   07  PTMODRES-SINKEI-NAME            PIC X(10).
      *            ž����ͳ
                   07  PTMODRES-TENKI                  PIC X(01).
                   07  PTMODRES-TENKI-NAME             PIC X(10).
      *            �ͻ������ʬ
                   07  PTMODRES-SISIKBN                PIC X(01).
                   07  PTMODRES-SISIKBN-NAME           PIC X(10).
      *
      *            ���Ƚ�̾��
                   07  PTMODRES-JIGYOUNAME             PIC X(80).
      *            �������ƻ�ܸ�̾
                   07  PTMODRES-PREFNAME               PIC X(20).
      *            �������ƻ�ܸ���ʬ
                   07  PTMODRES-PREFKBN                PIC X(01).
                   07  PTMODRES-PREFKBN-NAME           PIC X(02).
      *            ����Ϸ��Զ�̾
                   07  PTMODRES-CITYNAME               PIC X(80).
      *            ����Ϸ��Զ��ʬ
                   07  PTMODRES-CITYKBN                PIC X(01).
      *            ����Ϸ��Զ�̾��
                   07  PTMODRES-CITYKBN-NAME           PIC X(02).
      *            ϫ�ҥ쥻������� ���ǯ��
                   07  PTMODRES-KIJYUNYM               PIC X(07).
      *            ϫ�ҥ쥻������� ���
                   07  PTMODRES-RESEPUTO-COUNT         PIC X(03).
      *������
      *            �����������ʬ
                   07  PTMODRES-JIBAISEIKBN            PIC X(01).
                   07  PTMODRES-JIBAISEIKBN-NAME       PIC X(20).
      *            �������ݸ����̾
                   07  PTMODRES-HOKENNAME              PIC X(80).
      *            ������ô����
                   07  PTMODRES-RSI-DRCD               PIC X(05).
      *            ������ô����̾��
                   07  PTMODRES-RSI-DRCD-NAME          PIC X(100).
      *            �����ն�ʬ
                   07  PTMODRES-JIBAIKBN               PIC  X(01).
      *            �����ն�ʬ̾��
                   07  PTMODRES-JIBAIKBN-NAME          PIC  X(30).
      *���ե�������
      *            �򹯴�����Ģ�ֹ�
                   07  PTMODRES-KANRITCHO-NUMBER       PIC X(13).
      *            »����ʬ
                   07  PTMODRES-DAMAGE-CLASS           PIC X(03).
      *            »����ʬ̾��
                   07  PTMODRES-DAMAGE-CLASS-NAME      PIC X(100).
      *�軰�Թ԰�
      *            ��ʪ�ٵ��ʬ
                   07  PTMODRES-DS-GENBTKBN            PIC X(01).
                   07  PTMODRES-DS-GENBTKBN-NAME       PIC X(20).
      *            �õ������ʬ
                   07  PTMODRES-DS-TOKKIKBN            PIC X(01).
                   07  PTMODRES-DS-TOKKIKBN-NAME       PIC X(20).
      *
      *
      *      �ݸ��ȹ礻����
             03  PTMODRES-COMBINATION-INF-G.
               05  PTMODRES-COMBINATION-INF   OCCURS 200 TIMES.
      *            ������ʬ�ʿ���������Τߡ�
                   07  PTMODRES-COMB-MODE             PIC X(06).
      *            �ݸ��ȹ礻�ֹ�
                   07  PTMODRES-COMBINATION-NUMBER     PIC X(04).
      *            �ݸ��ȹ礻����ô����������
                   07  PTMODRES-COMBINATION-RATE-ADM   PIC X(04).
      *            �ݸ��ȹ礻����ô���ʳ����
                   07  PTMODRES-COMBINATION-RATE-OUT   PIC X(04).
      *            �ݸ��ȹ礻��ͭ��������
                   07  PTMODRES-COMB-START-DATE        PIC X(10).
      *            �ݸ��ȹ礻��ͭ����λ��
                   07  PTMODRES-COMB-END-DATE          PIC X(10).
      *
      *            �ݸ��ɣ�
                   07  PTMODRES-COMB-MAIN-ID           PIC X(10).
      *            �ݸ��μ���
                   07  PTMODRES-COMB-MAIN-CLASS        PIC X(03).
      *            �ݸ����ֹ�
                   07  PTMODRES-COMB-MAIN-NUMBER       PIC X(08).
      *            �ݸ��μ���̾��
                   07  PTMODRES-COMB-MAIN-NAME         PIC X(20).
      *            ����
                   07  PTMODRES-COMB-MARK              PIC X(80).
      *            �ֹ�
                   07  PTMODRES-COMB-NUMBER            PIC X(80).
      *            ����
                   07  PTMODRES-COMB-EDABAN            PIC X(02).
      *            ��³��ʬ
                   07  PTMODRES-COMB-CONTINUATION       PIC X(01).
      *            �����ʬ
                   07  PTMODRES-COMB-ASSISTANCE         PIC X(01).
      *            �����ʬ̾��
                   07  PTMODRES-COMB-ASSI-NAME          PIC X(20).
      *            �ܿͲ�²��ʬ
                   07  PTMODRES-COMB-FAMILY             PIC X(01).
      *            �������
                   07  PTMODRES-COMB-PUBLIC-INS-INF
                                              OCCURS  4 TIMES.
      *                ����ɣ�
                       09  PTMODRES-COMB-INSURANCE-ID     PIC X(10).
      *                ����μ���
                       09  PTMODRES-COMB-INSURANCE-CLASS  PIC X(03).
      *                ����μ���̾��
                       09  PTMODRES-COMB-INSURANCE-NAME   PIC X(20).
      *                ��ô���ֹ�
                       09  PTMODRES-COMB-PROVIDER-NUMBER  PIC X(08).
      *                ������ֹ�
                       09  PTMODRES-COMB-RECIPIENT-NUMBER PIC X(20).
      *                ��������ôΨ�ʳ��
                       09  PTMODRES-RATE-ADMISSION        PIC X(04).
      *                �����ݸ����
                       09  PTMODRES-MONEY-ADMISSION       PIC X(06).
      *                �������ôΨ�ʳ��
                       09  PTMODRES-RATE-OUTPATIENT       PIC X(04).
      *                ����ݸ����
                       09  PTMODRES-MONEY-OUTPATIENT      PIC X(06).
      *
      *      ���顼��å�����
             03  PTMODRES-PATIENT-ERRMSG-G.
               05  PTMODRES-PATIENT-ERRMSG-INF  OCCURS   50.
                   07  PTMODRES-ERROR          PIC X(04).
                   07  PTMODRES-ERROR-MSG      PIC X(200).
      *      ��˥󥰥�å�����
             03  PTMODRES-WARNING-MSG-G.
                 05  PTMODRES-WARNING-INF    OCCURS   50.
                     07  PTMODRES-WARNING      PIC X(04).
                     07  PTMODRES-WARNING-MSG  PIC X(200).
      *      ��ǧ��å�����
             03  PTMODRES-SELECT-MSG-G.
                 05  PTMODRES-SELECT-CODE      PIC X(04).
                 05  PTMODRES-SELECT-MSG       PIC X(200).
      *
