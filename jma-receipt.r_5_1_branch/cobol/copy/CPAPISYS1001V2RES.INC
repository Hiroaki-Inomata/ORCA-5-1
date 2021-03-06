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
      *    ���Уɥ쥹�ݥ󥹰��ŵ��ؾ������
      *                                 COPY (CPAPISYS1001V2RES.INC)
      ******************************************************************
        01  XML-SYSTEM1001RES.
            02  SYS101RES-CONTEXT            PIC S9(9)   BINARY.
            02  SYS101RES-OBJECT             PIC X(36).
            02  SYS101RES-MODE               PIC S9(9)   BINARY.
            02  SYS101RES-RECORDNAME         PIC X(128).
            02  SYS101RES-APPOINTRES.
      *       �»���
              03  SYS101RES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              03  SYS101RES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              03  SYS101RES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              03  SYS101RES-API-RESULT-MSG      PIC X(100).
      *
              03  SYS101RES-RESKEY              PIC X(100).
      *       �����
              03  SYS101RES-BASE-DATE           PIC X(10).
      *       ���ŵ��ؾ���
              03  SYS101RES-MEDICAL-INFOMATION.
      *           ��ƻ�ܸ��ֹ�
                  05  SYS101RES-PREFECTURES-NUMBER  PIC X(02).
      *           ����ɽ
                  05  SYS101RES-POINT-LIST          PIC X(01).
      *           ���ŵ��إ�����
                  05  SYS101RES-INSTITUTION-CODE    PIC X(07).
      *           ���ŵ��ؼ���
                  05  SYS101RES-INSTITUTION-SPECIATION PIC X(01).
      *           ���ŵ��أɣ�
                  05  SYS101RES-INSTITUTION-ID         PIC X(24).
      *           ���ŵ���̾��
                  05  SYS101RES-INSTITUTION-WHOLENAME  PIC X(120).
      *           û�̰��ŵ���̾��
                  05  SYS101RES-TANHOSPNAME     PIC X(20).
      *           ���߼�̾��
                  05  SYS101RES-ESTABLISHER-WHOLENAME  PIC X(120).
      *           ������̾��
                  05  SYS101RES-ADMINISTRATOR-WHOLENAME PIC X(80).
      *           �¾���
                  05  SYS101RES-BEDSU           PIC X(04).
      *           �¾����ʰ��̡�
                  05  SYS101RES-BEDSUIPN        PIC X(04).
      *           Ϸ�ͻ�ʧ��ʬ
                  05  SYS101RES-ROUPAYKBN       PIC X(01).
                  05  SYS101RES-ROUPAYKBNMEI    PIC X(06).
      *           �������±�
                  05  SYS101RES-HOSPSBT1        PIC X(01).
                  05  SYS101RES-HOSPSBT1MEI     PIC X(18).
      *           ����������ʬ
                  05  SYS101RES-SYOHOKBN        PIC X(01).
                  05  SYS101RES-SYOHOKBNMEI     PIC X(06).
      *           ���ŵ��إ����ɡʴ�����
                  05  SYS101RES-HOSPCDN         PIC X(40).
      *           ʬ�ڵ��ش����ֹ�
                  05  SYS101RES-SANKANUM        PIC X(10).
      *           �����ȯ�ԥե饰
                  05  SYS101RES-SKYPRTFLG       PIC X(01).
                  05  SYS101RES-SKYPRTFLGMEI    PIC X(28).
      *           ������������ȯ�ԥե饰
                  05  SYS101RES-SYOHOPRTFLG     PIC X(01).
                  05  SYS101RES-SYOHOPRTFLGMEI  PIC X(28).
      *           �������ɽ���ե饰
                  05  SYS101RES-ZENSYOHOFLG     PIC X(01).
                  05  SYS101RES-ZENSYOHOFLGMEI  PIC X(12).
      *           ���޾���ȯ�ԥե饰
                  05  SYS101RES-YAKUJYOPRTFLG   PIC X(01).
                  05  SYS101RES-YAKUJYOPRTFLGMEI
                                                PIC X(28).
      *           ���������ٽ�ȯ�Զ�ʬ
                  05  SYS101RES-MEIPRTFLG       PIC X(01).
                  05  SYS101RES-MEIPRTFLGMEI    PIC X(30).
      *           ������Ģȯ�ԥե饰
                  05  SYS101RES-OKUSURIPRTFLG   PIC X(01).
                  05  SYS101RES-OKUSURIPRTFLGMEI
                                                PIC X(34).
      *           ͽ��ɼȯ�ԥե饰
                  05  SYS101RES-YYKHYOPRTFLG    PIC X(01).
                  05  SYS101RES-YYKHYOPRTFLGMEI PIC X(18).
      *           �ǡ��������ե饰
                  05  SYS101RES-DATACREATEFLG   PIC X(01).
                  05  SYS101RES-DATACREATEFLGMEI
                                                PIC X(12).
      *           �ǡ��������ˡ��ʬ
                  05  SYS101RES-DATATEISYUTUKBN PIC X(01).
                  05  SYS101RES-DATATEISYUTUKBNMEI
                                                PIC X(12).
      *           �ǡ��������ɶ�ʬ
                  05  SYS101RES-DATAINFECTIONKBN
                                                PIC X(01).
                  05  SYS101RES-DATAINFECTIONKBNMEI
                                                PIC X(12).
      *           ���ȷ׻��оݶ�ʬ
                  05  SYS101RES-GENTAIKBN       PIC X(01).
                  05  SYS101RES-GENTAIKBNMEI    PIC X(20).
      *           �����ü������ʸ���ͭ��
                  05  SYS101RES-DISCOUNT-HASU-KBN
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBNNM
                                                PIC X(30).
      *           �����ü��������ݡʸ���̵���ݸ�ʬ��
                  05  SYS101RES-DISCOUNT-HASU-KBN2
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN2NM
                                                PIC X(30).
      *           �����ü��������ݡʸ���̵������ʬ��
                  05  SYS101RES-DISCOUNT-HASU-KBN2J
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN2JNM
                                                PIC X(30).
      *           �����ü������ϫ�ҡʸ���̵���ݸ�ʬ��
                  05  SYS101RES-DISCOUNT-HASU-KBN3
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN3NM
                                                PIC X(30).
      *           �����ü������ϫ�ҡʸ���̵������ʬ��
                  05  SYS101RES-DISCOUNT-HASU-KBN3J
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN3JNM
                                                PIC X(30).
      *           �����ü�����꼫���աʸ���̵���ݸ�ʬ��
                  05  SYS101RES-DISCOUNT-HASU-KBN4
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN4NM
                                                PIC X(30).
      *           �����ü�����꼫���աʸ���̵������ʬ��
                  05  SYS101RES-DISCOUNT-HASU-KBN4J
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN4JNM
                                                PIC X(30).
      *           �����ü����������ʸ���̵������ʬ��
                  05  SYS101RES-DISCOUNT-HASU-KBN5J
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN5JNM
                                                PIC X(30).
      *           �����ü�������軰�Թ԰١ʸ���̵���ݸ�ʬ��
                  05  SYS101RES-DISCOUNT-HASU-KBN6
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN6NM
                                                PIC X(30).
      *           �����ü�������軰�Թ԰١ʸ���̵������ʬ��
                  05  SYS101RES-DISCOUNT-HASU-KBN6J
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN6JNM
                                                PIC X(30).
      *           �軰�Թ԰١ʰ��������ô��۷׻���ʬ
                  05  SYS101RES-DAISANKBN       PIC X(01).
                  05  SYS101RES-DAISANKBNMEI    PIC X(30).
      *           ������ü������
                  05  SYS101RES-TAX-HASU-KBN    PIC X(01).
                  05  SYS101RES-TAX-HASU-KBNMEI PIC X(30).
      *           �����ݸ��������ʬ
                  05  SYS101RES-JIHIPOSITION    PIC X(01).
                  05  SYS101RES-JIHIPOSITIONMEI PIC X(38).
      *           ���������ݸ��ֹ楿�ֶ�ʬ
                  05  SYS101RES-CHIHOUNUMKBN    PIC X(01).
                  05  SYS101RES-CHIHOUNUMKBNMEI PIC X(08).
      *           �������������ٳ�����׻���ʬ
                  05  SYS101RES-HIWARIKBN       PIC X(01).
                  05  SYS101RES-HIWARIKBNMEI    PIC X(18).
      *           ���񥳡���ü������
                  05  SYS101RES-JIHI-HASU-KBN   PIC X(01).
                  05  SYS101RES-JIHI-HASU-KBNMEI
                                                PIC X(38).
      *           ���ŵ��ؽ����
                  05  SYS101RES-ADDRESS-INFORMATION.
      *           �����
                      07  SYS101RES-WHOLEADDRESS       PIC X(200).
      *           ͹���ֹ�
                      07  SYS101RES-ADDRESS-ZIPCODE    PIC X(07).
      *           �����ֹ�
                      07  SYS101RES-PHONENUMBER        PIC X(15).
      *           FAX�ֹ�
                      07  SYS101RES-FAXNUMBER          PIC X(15).
      *           ��᡼�륢�ɥ쥹
                      07  SYS101RES-EMAIL-ADDRESS      PIC X(100).
      *           �ȣХ��ɥ쥹
                      07  SYS101RES-HOMEPAGE-ADDRESS   PIC X(100).
      *
