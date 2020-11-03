Work Log of Holger for Tradelane Solutions
==========================================

To-Do
-----

1. Issue [#1][1]: Include an unread `message-count` with the [XEP-0357][0357]
   `:summary` form.
2. Issue [#2][2]: Support [XEP-0430][0430]: Inbox.
3. Possibly add support for acknowledging outgoing messages with the
   [XEP-0359][0359] stanza ID.

Design Considerations
---------------------

- The unread message count included with push notifications should use the data
  gathered for Inbox support, so the first step is gathering that data.
- Main question is whether to store that Inbox data within the (MAM) `archive`
  or in a separate table.
- Another question is whether to support marking _parts_ of a conversation as
  read: If there's four unread messages and a [chat marker][0333] acknowledges
  only the older two of those, should the unread message count be updated from
  `4` to `2`, or is it acceptable for the count to remain at `4` (as this might
  be a rare corner case in practice)?
- And (how to) support remote MUC/MIX archives.

The database layout strongly depends on the answers to these questions. For the
moment, we'll go for a separate table which only tracks the most-recent message
and the unread count per conversation. The unread count is reset to `0` if
either the most-recent message is acknowledged with a `displayed`
[marker][0333], or if an outgoing chat message is sent to the peer.

Completed Tasks
---------------

| Task                                                | Hours         | Total Hours |
| -------------------------------------------------   | -------------:| -----------:|
| Initial design considerations and discussions       |           2.5 |         2.5 |
| XEP-0333 [support][X1] for `xmpp` library           |           0.5 |         3.0 |
| Initial [import][X2] of a bare-bones `mod_inbox`    |           0.5 |         3.5 |
| Add `mod_inbox_sql` for SQL storage of Inbox data   |           3.0 |         6.5 |
| Let `mod_inbox` write incoming messages to Inbox    |           1.0 |         7.5 |
| Let `mod_inbox` parse outgoing ACKs to update Inbox |           2.0 |         9.5 |
| Let `mod_push` include number unread messages       |           1.0 |        10.5 |
| Implement caching                                   |           1.5 |        12.0 |
| Test the new functionality and fix issues           |           1.0 |        13.0 |
| Add API call for expiring old inboxes               |           0.5 |        13.5 |

Next Tasks
----------

- Close issue [#1][1] and start working on issue [#2][2].

To-Do for Upstreaming
---------------------

- Sign ProcessOne Contribution License Agreement (CLA): <https://cla.process-one.net>
- Add support for remote groupchat archives.
- Add `inbox` table schemas for MySQL/MariaDB, SQLite, and Microsoft SQL.
- Implement `mod_inbox_mnesia`.
- Implement `mod_inbox_sql:export/1` function to convert Mnesia data to SQL.
- Add test cases for the new Inbox and push notification functionality.

[1]: https://github.com/simpl0/ejabberd/issues/1
[2]: https://github.com/simpl0/ejabberd/issues/2

[0333]: https://xmpp.org/extensions/xep-0333.html
[0357]: https://xmpp.org/extensions/xep-0357.html
[0359]: https://xmpp.org/extensions/xep-0359.html
[0430]: https://xmpp.org/extensions/xep-0430.html

[6120]: https://xmpp.org/rfcs/rfc6120.html

[X1]: https://github.com/simpl0/xmpp/commit/c48f6cb9f9cc419844c789ec47a79003aecb1814
[X2]: https://github.com/simpl0/ejabberd/commit/f63673b96f21e0612e145fe7deeb74e8bdacecf1
