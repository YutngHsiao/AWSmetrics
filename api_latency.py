import boto3
from datetime import timedelta
from datetime import datetime
import time
import os

region = 'eu-west-2'
ACCESS_KEY = 'AKIAI3I45XRHMDD2V6NA'
SECRET_KEY = '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'
c_time = datetime.utcnow()
client = boto3.client('cloudwatch',region_name=region, aws_access_key_id=ACCESS_KEY, aws_secret_access_key=SECRET_KEY)

def aws_cw(type, instence, win1=False, win2=False, Namespace=False, MetricName=False, Unit=False, Period=False):
    response = client.get_metric_statistics(
        Namespace=Namespace,
        MetricName=MetricName,
        Dimensions=[
            {
                'Name': type,
                'Value': instence
            },
        ],
        StartTime = c_time - timedelta(minutes=win1),
        EndTime = c_time - timedelta(minutes=win2),
        Period=Period,
        Statistics=['Average'],
        Unit=Unit
    )
    dp = response["Datapoints"]
    dic = []
    for i in dp:
        t = i['Timestamp']
        t = time.mktime(t.timetuple())
        dic.append({"time": t, "avg": i["Average"]})
    return dic
