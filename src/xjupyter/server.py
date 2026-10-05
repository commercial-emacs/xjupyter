# -*- coding: utf-8 -*-
# pylint: disable=wrong-import-position

from __future__ import unicode_literals
from __future__ import print_function

import logging
import datetime
import typing as t
from time import time
#from functools import partial
from queue import Empty

from jupyter_client.manager import KernelManager, start_new_kernel
from jupyter_client.blocking.client import BlockingKernelClient

class Server():
    kc: BlockingKernelClient
    km: KernelManager
    cell_by_msg_id: t.Dict[str, str]

    def __init__(self, **kwargs):
        log_level = kwargs.pop('log_level', logging.NOTSET)
        logging.basicConfig(level=log_level,
                            filename='/dev/null',
                            format='%(asctime)s %(levelname)s %(message)s',
                            datefmt='%Y-%m-%d %H:%M:%S')
        log_prefix = kwargs.pop('log_prefix', None)
        if log_prefix:
            stamp = datetime.datetime.fromtimestamp(time()).strftime('%Y%m%d.%H%M%S')
            logging.getLogger().addHandler(logging.FileHandler(log_prefix + stamp))

        self.km, self.kc = start_new_kernel()
        self.cell_by_msg_id = dict()

    @classmethod
    def _isoformatic(cls, u):
        for k, v in u.items():
            if isinstance(v, datetime.datetime):
                u[k] = v.isoformat()
            elif isinstance(v, dict):
                cls._isoformatic(v)
        return u

    def iopub_msg(self):
        # get_msgs() polls forever cuz timeout=None
        try:
            msg = self.kc.iopub_channel.get_msg(timeout=0)
        except Empty:
            return

        try:
            msg_id = msg["parent_header"]["msg_id"]
            cell_id = self.cell_by_msg_id[msg_id]
            yield (cell_id, self._isoformatic(msg))
        except KeyError:
            return

    def shell_msg(self):
        # get_msgs() polls forever cuz timeout=None
        try:
            msg = self.kc.shell_channel.get_msg(timeout=0)
        except Empty:
            return

        try:
            msg_id = msg["parent_header"]["msg_id"]
            cell_id = self.cell_by_msg_id[msg_id]
            yield (cell_id, self._isoformatic(msg))
        except KeyError:
            return

    def control_msg(self):
        # get_msgs() polls forever cuz timeout=None
        try:
            msg = self.kc.control_channel.get_msg(timeout=0)
            yield self._isoformatic(msg)
        except Empty:
            return

    def execute(self, id, **kwargs):
        # kwargs['output_hook'] = partial (rpc.handle_iopub, id)
        # return self._isoformatic(self.kc.execute_interactive(**kwargs))
        msg_id = self.kc.execute(
            kwargs['code'],
            allow_stdin=False,
        )
        self.cell_by_msg_id[msg_id] = id
        return None

    def interrupt_request(self, _id, **kwargs):
        # kwargs['output_hook'] = partial (rpc.handle_iopub, id)
        # return self._isoformatic(self.kc.execute_interactive(**kwargs))
        return self.km.interrupt_kernel()

# stackoverflow.com/questions/9977446/connecting-to-a-remote-ipython-instance
# discourse.jupyter.org/t/how-can-i-do-a-basic-hello-world-using-the-jupyter-client-api
# github.com/jupyter/jupyter_kernel_test/blob/main/jupyter_kernel_test/__init__.py
